# trial_flow.py — TrialFlowV5 (E-gate centered vertically; R→E seamless; uniform wrap; slow intervals)
# Manim Community Edition >= 0.19

from manim import *
import numpy as np
import random
from manim import config, rate_functions

config.disable_caching = True
MIN_FRAMES = 2  # enforce >=2 frames per segment so updaters advance


class TrialFlowV5(MovingCameraScene):
    """
    • Uniform one-lane patient flow; pauses only during R-gate/E-gate close-ups and milestones.
    • Start: no one past R-gate at t=0. R→E close-ups are chained (no main-stage flash).
    • E-gate: dose label below gates; endpoint label above middle gates with underline ~ text width.
    • Milestones: stage visible but lane PAUSED; steps centered in one row; proper cleanup.
    • FIX: uniform wrap (SPACING*N) so spacing never drifts.
    • NEW: E-gate close-up is vertically centered on screen (camera at origin, lane y=0).
    """

    # palette
    COL_A = "#377eb8"      # Dose A
    COL_B = "#ff7f00"      # Dose B
    COL_C = "#4daf4a"      # Dose C
    COL_GRAY = "#bdbdbd"
    COL_BLACK = "#111111"

    def __init__(self, **kwargs):
        super().__init__(**kwargs)
        # ------- Preview knobs -------
        self.preview = True             # set False for final
        self.preview_level = 1
        self.time_scale = 0.55          # set 1.0 for final
        self.flow_speed = 1.00
        self.flow_running = True

        # Global interval scaler (ALL waits / lingers / gaps × this factor)
        self.interval_scale = 4.0

        # R-gate event plumbing
        self.force_next_arm = None
        self.last_cross_event = None    # {"id": int, "arm": "A/B/C", "person": Mobject}
        self.event_id = 0

        # Stage cache to chain R→E without black frames
        self.hidden_stage = None

        # ===== E-gate layout: centered composition =====
        self.egate_y = 0.0            # lane y for E-gate close-up (center line)
        self.ep_label_offset = 0.50   # label offset above gates

    # helper: scale any interval consistently
    def I(self, t): return t * self.interval_scale

    def is_fast(self, level=1): return self.preview and self.preview_level >= level

    # global time scaling + 2-frame floor
    def play(self, *args, **kwargs):
        rt = kwargs.get("run_time", None)
        if rt is not None:
            min_rt = max(1.0 / config.frame_rate, MIN_FRAMES / config.frame_rate)
            kwargs["run_time"] = max(rt * self.time_scale, min_rt)
        return super().play(*args, **kwargs)

    def wait(self, duration=0.0, *args, **kwargs):
        if duration and duration > 0:
            min_rt = max(1.0 / config.frame_rate, MIN_FRAMES / config.frame_rate)
            duration = max(duration * self.time_scale, min_rt)
        return super().wait(duration, *args, **kwargs)

    # ======================================================================
    # Main scene
    # ======================================================================
    def construct(self):
        random.seed(42)
        self.allowed_arms = ["A", "B", "C"]  # after MS1 -> ["A","B"]

        self.x_min, self.x_max = -6.3, 6.3
        self.y0 = -0.5
        def x_at(fr): return self.x_min + fr * (self.x_max - self.x_min)
        self.x_rg = x_at(1/3); self.x_l1 = x_at(1/2); self.x_l2 = x_at(2/3)

        self.camera.frame.width = 15

        baseline = Line([self.x_min, self.y0, 0], [self.x_max, self.y0, 0], stroke_opacity=0.18)
        title = Text("Trial flow — single lane • R-gate • 4 endpoints • 2 milestones", weight=BOLD).scale(0.45).to_edge(UP)
        self.add(baseline, title)

        # R-gate with three lamps
        r_gate, lamps = self._build_random_gate(self.x_rg, self.y0)
        self.r_gate_lamps = lamps
        self.add(r_gate)

        # lane (fixed spacing; uniform speed; ensure no one past R-gate at t=0)
        SPACING = 0.56
        N = 26 if self.is_fast(1) else 34
        LANE_CYCLE = SPACING * N  # exact wrap distance to preserve spacing
        self.lane_offset = ValueTracker(0.0)
        self.persons = VGroup(*[self._make_person(self.COL_GRAY).scale(0.95) for _ in range(N)])
        base_start = self.x_rg - N * SPACING - 0.6  # small margin; max initial x < x_rg
        x_anchors = [base_start + i * SPACING for i in range(N)]

        def lane_updater(group, dt):
            eff_dt = (1.0 / config.frame_rate) if self.flow_running else 0.0
            self.lane_offset.increment_value(eff_dt * self.flow_speed)
            off = self.lane_offset.get_value()
            for k, p in enumerate(group):
                x = x_anchors[k] + off
                if x > self.x_max + SPACING:
                    x -= LANE_CYCLE
                    x_anchors[k] = x - off
                    p.arm_assigned = None
                    self._set_person_color(p, self.COL_GRAY)
                p.move_to([x, self.y0, 0])
                if p.arm_assigned is None and x >= self.x_rg:
                    chosen = self.force_next_arm if self.force_next_arm else random.choice(self.allowed_arms)
                    self.force_next_arm = None
                    p.arm_assigned = chosen
                    self._set_person_color(p, {"A": self.COL_A, "B": self.COL_B, "C": self.COL_C}[chosen])
                    self.event_id += 1
                    self.last_cross_event = {"id": self.event_id, "arm": chosen, "person": p}

        self.persons.add_updater(lane_updater)
        self.add(self.persons)

        # short calm start; the long "pre-demo" wait happens before the FIRST R-cutaway below
        self.wait(self.I(0.8))

        seq_arms = ["A", "B", "C"]
        color_of = {"A": self.COL_A, "B": self.COL_B, "C": self.COL_C}

        for idx, arm in enumerate(seq_arms):
            # wait for next assignment of this arm at R-gate
            if idx == 0:
                self._wait_for_arm_at_gate(arm, min_gap=self.I(0.6))
                self.wait(self.I(2.2))   # extra long delay before first R-cutaway (let many get colored)
            else:
                self._wait_for_arm_at_gate(arm, min_gap=self.I(1.8))

            # R-gate cutaway (do NOT restore stage, so we can jump to E-gate seamlessly)
            self._rg_cutaway(color_of[arm], scan_rt=1.1, dwell_rt=self.I(0.70), restore_stage=False)

            # E-gate close-up; stage already hidden by R-gate
            self._zoom_endpoints_for_arm(label=f"Dose {arm}", color=color_of[arm], pre_hidden=True)

        # Listener 1 -> MS1 (drop C)
        listener1 = self._spawn_listener(target_x=self.x_l1, rt=self.I(1.6))
        self._milestone_bubble_sequential(anchor_x=self.x_l1, ms_index=1, drop_C=True)
        anchor1 = self._listener_to_anchor(listener1, color=self.COL_A); self.add(anchor1)

        self.wait(self.I(1.6))  # longer gap

        # Listener 2 -> MS2
        listener2 = self._spawn_listener(target_x=self.x_l2, rt=self.I(1.6))
        self._milestone_bubble_sequential(anchor_x=self.x_l2, ms_index=2, drop_C=False)
        anchor2 = self._listener_to_anchor(listener2, color=self.COL_B); self.add(anchor2)

        self.wait(self.I(0.9))
        self.play(FadeOut(VGroup(self.persons, r_gate, anchor1, anchor2, baseline, title), run_time=0.8))

    # ===== wait for arm at R-gate ==========================================================
    def _wait_for_arm_at_gate(self, arm, min_gap=0.8):
        prev_id = self.event_id
        self.force_next_arm = arm
        self.flow_running = True
        while True:
            self.wait(0.02)  # tiny polling tick
            ev = self.last_cross_event
            if ev and ev["id"] > prev_id and ev["arm"] == arm:
                break
        if min_gap > 0:
            self.wait(min_gap)

    # ======================================================================
    # People / badges
    # ======================================================================
    def _make_person(self, color):
        head = Circle(radius=0.16, fill_color=color, fill_opacity=1, stroke_width=0)
        body = RoundedRectangle(width=0.34, height=0.50, corner_radius=0.10,
                                fill_color=color, fill_opacity=1, stroke_width=0)
        body.next_to(head, DOWN, buff=0.06)
        g = VGroup(head, body); g.arm_assigned = None
        return g

    def _set_person_color(self, person, color):
        person[0].set_fill(color); person[1].set_fill(color)

    def _badge_anchor_on_chest_idx(self, person, idx):
        torso_top = person[1].get_top()
        chest_y = torso_top[1] - 0.12
        base_x = person.get_center()[0]
        offsets_x = [-0.21, -0.07, 0.07, 0.21]
        return np.array([base_x + offsets_x[idx], chest_y, 0.0])

    # ======================================================================
    # R-gate & cutaway (optionally NOT restoring stage)
    # ======================================================================
    def _build_random_gate(self, x, y):
        frame = RoundedRectangle(corner_radius=0.2, width=1.1, height=1.6,
                                 stroke_color=GREY, stroke_width=2, fill_opacity=0).move_to([x, y, 0])
        band = Rectangle(width=0.95, height=0.20, fill_color=GREY, fill_opacity=0.07, stroke_opacity=0)\
            .move_to(frame.get_center())
        lamp_A = Circle(radius=0.07, fill_color=self.COL_A, fill_opacity=1, stroke_width=0)\
            .move_to(frame.get_top() + DOWN * 0.18 + LEFT * 0.28)
        lamp_B = Circle(radius=0.07, fill_color=self.COL_B, fill_opacity=1, stroke_width=0)\
            .move_to(frame.get_top() + DOWN * 0.18 + ORIGIN)
        lamp_C = Circle(radius=0.07, fill_color=self.COL_C, fill_opacity=1, stroke_width=0)\
            .move_to(frame.get_top() + DOWN * 0.18 + RIGHT * 0.28)
        return VGroup(frame, band, lamp_A, lamp_B, lamp_C), {"A": lamp_A, "B": lamp_B, "C": lamp_C}

    def _rg_cutaway(self, arm_color_hex, scan_rt=1.1, dwell_rt=0.70, restore_stage=True):
        # pause & hide main stage
        self.flow_running = False
        stage = Group(*[m for m in self.mobjects if m is not self.camera.frame])
        self.play(FadeOut(stage, run_time=0.2))

        # remember what we hid (so E-gate can restore later)
        if not restore_stage:
            self.hidden_stage = stage
        else:
            self.hidden_stage = None

        frame = RoundedRectangle(corner_radius=0.25, width=5.6, height=3.0,
                                 stroke_color=GREY, stroke_width=4, fill_opacity=0).move_to(ORIGIN)
        band = Rectangle(width=5.0, height=0.32, fill_color=GREY, fill_opacity=0.10, stroke_opacity=0)\
            .move_to(frame.get_center())
        demo = self._make_person(self.COL_GRAY).scale(1.6).move_to(frame.get_left() + RIGHT * 1.0)
        scan = Rectangle(width=0.22, height=2.7, fill_color=YELLOW, fill_opacity=0.20, stroke_opacity=0)\
            .move_to(frame.get_left() + RIGHT * 0.5)

        self.play(FadeIn(VGroup(frame, band, demo), run_time=0.25))
        self.add(scan)
        self.play(scan.animate.move_to(frame.get_right() + LEFT * 0.5), run_time=scan_rt, rate_func=linear)
        self._set_person_color(demo, arm_color_hex)
        self.wait(dwell_rt)

        self.play(FadeOut(VGroup(frame, band, demo, scan), run_time=0.2))

        # optional immediate restore (normally we skip to chain into E-gate)
        if restore_stage:
            self.play(FadeIn(stage, run_time=0.2))
            self.flow_running = True

    # ======================================================================
    # Endpoint plaques & per-arm close-up (stage may already be hidden)
    # ======================================================================
    def _endpoint_icon(self, kind):
        if kind == "binary":
            return Text("+/-", weight=BOLD, color=WHITE).scale(0.7)
        if kind == "gauss":
            # Q1 axes + slightly higher Gaussian
            x0, y0 = -0.30, -0.18; x1, y1 = 0.30, 0.18
            x_axis = Line([x0, y0, 0], [x1, y0, 0], stroke_width=2, color=GREY_D)
            y_axis = Line([x0, y0, 0], [x0, y1, 0], stroke_width=2, color=GREY_D)
            L = (x1 - x0)
            xs = np.linspace(0.0, L * 0.92, 120)
            mu = L * 0.52; sigma = L * 0.33; amp = (y1 - y0) * 0.54
            pts = [np.array([x0 + t, y0 + amp * np.exp(-((t - mu)**2)/(2*sigma**2)), 0]) for t in xs]
            curve = VMobject(stroke_color=WHITE, stroke_width=4).set_points_smoothly(pts)
            area_pts = [*pts, np.array([x0 + xs[-1], y0, 0]), np.array([x0 + xs[0], y0, 0])]
            area = VMobject(fill_color=GREY_B, fill_opacity=0.18, stroke_opacity=0.0).set_points_as_corners(area_pts)
            return VGroup(area, curve, x_axis, y_axis)
        if kind == "tte":
            outer = RoundedRectangle(corner_radius=0.10, width=0.90, height=0.62,
                                     stroke_color=WHITE, stroke_width=4, fill_opacity=0)
            header = Rectangle(width=0.90, height=0.16, fill_color=WHITE, fill_opacity=1, stroke_width=0)\
                .move_to(outer.get_top() + DOWN * 0.08)
            grid = VGroup()
            for dx in [-0.24, 0.0, 0.24]:
                grid.add(Line([dx, -0.18, 0], [dx, 0.14, 0], stroke_color=WHITE, stroke_width=3))
            for dy in [-0.02, 0.18]:
                grid.add(Line([-0.40, dy, 0], [0.40, dy, 0], stroke_color=WHITE, stroke_width=3))
            return VGroup(outer, header, grid)
        if kind == "spark":
            pts = [LEFT*0.30+DOWN*0.08, LEFT*0.15+UP*0.10, ORIGIN+DOWN*0.02,
                   RIGHT*0.15+UP*0.12, RIGHT*0.30+DOWN*0.05]
            return VMobject(stroke_color=WHITE, stroke_width=5).set_points_as_corners(pts)
        return Square(0.2, color=WHITE, stroke_width=5)

    def _build_endpoint_gate(self, x, y, kind):
        frame = RoundedRectangle(corner_radius=0.2, width=1.3, height=1.9,
                                 stroke_color=GREY, stroke_width=2, fill_opacity=0).move_to([x, y, 0])
        plaque = RoundedRectangle(corner_radius=0.14, width=0.95, height=0.68,
                                  fill_color=BLACK, fill_opacity=1.0, stroke_width=0)\
            .move_to(frame.get_top() + DOWN * 0.25)
        ico = self._endpoint_icon(kind).scale(0.60).move_to(plaque.get_center())
        return VGroup(frame, plaque, ico)

    def _zoom_endpoints_for_arm(self, label, color, pre_hidden=False):
        # if stage not already hidden by R-gate, hide it now and remember
        if not pre_hidden:
            self.flow_running = False
            stage_hidden_now = Group(*[m for m in self.mobjects if m is not self.camera.frame])
            self.play(FadeOut(stage_hidden_now, run_time=0.2))
            self.hidden_stage = stage_hidden_now

        # camera centered (landscape-friendly): aim at origin, fixed width
        self.camera.frame.save_state()
        target_rect = Rectangle(width=8.8, height=4.2, stroke_opacity=0).move_to(ORIGIN)
        self.play(self.camera.frame.animate.move_to(target_rect).set(width=9.2), run_time=0.35)

        # lane & gates (centered y)
        y = self.egate_y
        lane = Line(LEFT * 4.4 + UP * y, RIGHT * 4.4 + UP * y, stroke_opacity=0.15)
        self.add(lane)

        xs = [-3.0, -1.0, 1.0, 3.0]
        kinds = ["binary", "gauss", "tte", "spark"]
        gates = VGroup(*[self._build_endpoint_gate(x, y, k) for x, k in zip(xs, kinds)])
        self.add(gates)

        # Dose label fully BELOW the gates
        gates_bottom = min(g.get_bottom()[1] for g in gates)
        tag = Text(label, weight=BOLD, color=color).scale(0.5)
        tag.move_to([0, gates_bottom - 0.40, 0])
        self.add(tag)

        # Endpoint type label ABOVE middle gates with underline ~ label width + padding
        def show_ep_label(txt, arm_color):
            center_x = (xs[1] + xs[2]) / 2
            mid_top_y = max(gates[1].get_top()[1], gates[2].get_top()[1])
            lbl = Text(txt, weight=BOLD, color=WHITE).scale(0.56)
            pad = 0.18
            L = lbl.width + 2 * pad
            underline = Line([0 - L/2, 0, 0], [0 + L/2, 0, 0], stroke_width=6, color=arm_color, stroke_opacity=0.9)
            group = VGroup(lbl, underline).arrange(DOWN, buff=0.06)
            group.move_to([center_x, mid_top_y + self.ep_label_offset, 0])
            self.play(FadeIn(group, run_time=0.18))
            return group

        # demo patient
        demo = self._make_person(color).scale(1.05).move_to([xs[0] - 1.2, y, 0])
        self.add(demo)

        # chest badge flash (then disappear)
        def flash_badge(idx):
            badge = Circle(radius=0.11, fill_color=WHITE, fill_opacity=1, stroke_width=0)
            badge.move_to(self._badge_anchor_on_chest_idx(demo, idx))
            self.add(badge)
            self.play(FadeIn(badge, scale=0.85, run_time=0.12))
            self.play(Flash(badge, flash_radius=0.25, line_length=0.10), run_time=0.12)
            self.play(FadeOut(badge, run_time=0.14))

        ep_names = ["binary", "continuous", "time-to-event", "longitudinal"]

        # arrive → dwell → label + badge → clear → next
        for i, (x, k) in enumerate(zip(xs, kinds)):
            self.play(demo.animate.move_to([x, y, 0]), run_time=0.42, rate_func=linear)
            self.wait(self.I(0.40))
            lbl_grp = show_ep_label(ep_names[i], color)
            flash_badge(i)
            self.play(FadeOut(lbl_grp, run_time=0.18))

        # clean up & restore stage & flow
        self.play(FadeOut(VGroup(lane, tag, gates, demo), run_time=0.25))
        self.play(self.camera.frame.animate.restore(), run_time=0.35)
        if self.hidden_stage is not None:
            self.play(FadeIn(self.hidden_stage, run_time=0.2))
            self.hidden_stage = None
        self.flow_running = True

    # ======================================================================
    # Listener & milestone (keep stage visible; pause flow)
    # ======================================================================
    def _build_listener(self):
        head = Circle(radius=0.10, fill_color=WHITE, fill_opacity=1, stroke_color=self.COL_BLACK, stroke_width=2)
        body = RoundedRectangle(width=0.36, height=0.52, corner_radius=0.10,
                                fill_color=WHITE, fill_opacity=1, stroke_color=self.COL_BLACK, stroke_width=2)
        body.next_to(head, DOWN, buff=0.05)
        arm = Line(body.get_top()+DOWN*0.18+LEFT*0.15, body.get_top()+DOWN*0.05+RIGHT*0.18,
                   stroke_color=self.COL_BLACK, stroke_width=2)
        return VGroup(head, body, arm)

    def _spawn_listener(self, target_x, rt=1.6):
        listener = self._build_listener().move_to([self.x_min - 1.2, self.y0 + 0.9, 0]).scale(1.0)
        self.add(listener)
        self.play(listener.animate.move_to([target_x, self.y0 + 0.9, 0]),
                  run_time=rt, rate_func=rate_functions.smooth)
        return listener

    def _listener_to_anchor(self, listener, color=BLUE):
        x = listener.get_center()[0]
        line = Line([x, self.y0, 0], [x, self.y0 + 0.85, 0], stroke_color=color, stroke_width=3)
        node = Circle(radius=0.12, fill_color=color, fill_opacity=1, stroke_width=0).move_to([x, self.y0 + 0.85, 0])
        check = VGroup(
            Line(node.get_center()+LEFT*0.07+DOWN*0.02, node.get_center()+ORIGIN+DOWN*0.08, stroke_width=3, color=WHITE),
            Line(node.get_center()+ORIGIN+DOWN*0.08, node.get_center()+RIGHT*0.09+UP*0.06, stroke_width=3, color=WHITE)
        )
        plate = VGroup(node, check)
        self.play(Transform(listener, node), run_time=0.30)
        self.play(Create(line, run_time=0.32))
        if not self.is_fast(1):
            self.play(Flash(plate, flash_radius=0.4, line_length=0.15), run_time=0.20)
        self.remove(listener)
        return VGroup(line, plate)

    def _milestone_bubble_sequential(self, anchor_x, ms_index, drop_C):
        prev = self.flow_running; self.flow_running = False

        self.camera.frame.save_state()
        focus = Rectangle(width=8.0, height=4.5, stroke_opacity=0).move_to([anchor_x, self.y0 + 1.6, 0])
        self.play(self.camera.frame.animate.move_to(focus).set(width=8.6), run_time=0.38)

        bubble = RoundedRectangle(corner_radius=0.25, width=6.8, height=3.0,
                                  stroke_color=GREY, fill_color=WHITE, fill_opacity=1.0)\
            .move_to([anchor_x, self.y0 + 1.6, 0])
        tail = Polygon(bubble.get_bottom()+DOWN*0.2, bubble.get_bottom()+RIGHT*0.4+DOWN*0.2,
                       bubble.get_bottom()+RIGHT*0.18+UP*0.02, color=GREY, stroke_width=2,
                       fill_color=WHITE, fill_opacity=1.0)
        title = Text(f"Milestone {ms_index}", weight=BOLD).scale(0.46)\
            .next_to(bubble.get_top(), DOWN, buff=0.18).move_to([anchor_x, bubble.get_top()[1]-0.35, 0])
        panel = VGroup(bubble, tail, title)
        self.play(FadeIn(panel, run_time=0.22))

        center = np.array([anchor_x, bubble.get_center()[1] - 0.10, 0])

        def show_step_centered(label_txt, icon_builder, linger=1.35, keep=False):
            icon = icon_builder()
            label = Text(label_txt, weight=MEDIUM, color="#222").scale(0.46)
            row = VGroup(icon, label).arrange(RIGHT, buff=0.25).move_to(center)
            self.play(FadeIn(row, run_time=0.20))
            self.wait(self.I(linger))
            if not keep: self.play(FadeOut(row, run_time=0.18))
            return row

        # 1) Data snapshot (camera + table) + local flash
        def snapshot_icon():
            cam = VGroup(Rectangle(width=0.62, height=0.36, fill_color=BLACK, fill_opacity=1, stroke_width=0),
                         Circle(radius=0.11, color=WHITE, fill_opacity=0, stroke_width=3))
            table = self._table_icon().scale(0.92)
            return VGroup(cam, table).arrange(RIGHT, buff=0.28)

        row = show_step_centered("Data snapshot", snapshot_icon, linger=1.45, keep=True)
        flash = RoundedRectangle(corner_radius=0.22, width=bubble.width-0.3, height=bubble.height-0.3,
                                 fill_color=WHITE, fill_opacity=0.0, stroke_opacity=0).move_to(bubble.get_center())
        self.add(flash)
        self.play(flash.animate.set_fill(opacity=1.0), run_time=0.12)
        self.play(flash.animate.set_fill(opacity=0.0), run_time=0.12)
        self.play(FadeOut(row, run_time=0.18))

        # 2) Stat analysis (bar plot)
        def stat_icon(): return self._bars_icon().scale(0.92)
        show_step_centered("Stat analysis", stat_icon, linger=1.40)

        # 3) Decision (MS1 only) — drop C (dot crossed and grayed) + lamp C grayed + allowed_arms -> A/B
        if drop_C:
            def decision_icon_dots():
                dotA = Circle(radius=0.22, fill_color=self.COL_A, fill_opacity=1, stroke_width=0)
                dotB = Circle(radius=0.22, fill_color=self.COL_B, fill_opacity=1, stroke_width=0)
                dotC = Circle(radius=0.22, fill_color=self.COL_C, fill_opacity=1, stroke_width=0)
                return VGroup(dotA, dotB, dotC).arrange(RIGHT, buff=0.40)
            row = show_step_centered("Decision", decision_icon_dots, linger=0.90, keep=True)
            dots = row[0]
            if isinstance(dots, VGroup) and len(dots.submobjects) >= 3:
                C = dots[2]
                cross = VGroup(
                    Line(C.get_center()+LEFT*0.16+UP*0.16, C.get_center()+RIGHT*0.16+DOWN*0.16, stroke_color=RED, stroke_width=6),
                    Line(C.get_center()+LEFT*0.16+DOWN*0.16, C.get_center()+RIGHT*0.16+UP*0.16, stroke_color=RED, stroke_width=6)
                )
                self.play(FadeIn(cross, run_time=0.20))
                self.wait(self.I(0.45))
                self.play(C.animate.set_fill(self.COL_GRAY, opacity=0.35), run_time=0.28)
                self.play(self.r_gate_lamps["C"].animate.set_fill(self.COL_GRAY, opacity=0.35), run_time=0.24)
                self.allowed_arms = ["A", "B"]
                self.wait(self.I(0.40))
                self.play(FadeOut(VGroup(row, cross), run_time=0.18))
            else:
                self.play(FadeOut(row, run_time=0.18))

        # 4) Save results (folder + page slide-in)
        def save_icon(): return self._save_folder_icon().scale(1.0)
        row = show_step_centered("Save results", save_icon, linger=1.45, keep=True)
        folder = row[0]
        if hasattr(folder, "page"):
            self._save_folder_anim(folder)
        self.wait(self.I(0.30))
        self.play(FadeOut(row, run_time=0.18))

        # close bubble & restore camera; RESUME flow
        self.play(FadeOut(panel, run_time=0.20))
        self.play(self.camera.frame.animate.restore(), run_time=0.32)
        self.flow_running = prev

    # ======================================================================
    # Small icons
    # ======================================================================
    def _table_icon(self):
        g = VGroup()
        base = Rectangle(width=1.0, height=0.6, stroke_color=GREY, stroke_width=2); g.add(base)
        for dx in [-0.33, 0.0, 0.33]:
            g.add(Line([dx, -0.3, 0], [dx, 0.3, 0], stroke_color=GREY, stroke_width=1.5))
        for dy in [-0.2, 0.0, 0.2]:
            g.add(Line([-0.5, dy, 0], [0.5, dy, 0], stroke_color=GREY, stroke_width=1.5))
        return g

    def _bars_icon(self):
        g = VGroup()
        base = Line([-0.6, -0.25, 0], [0.6, -0.25, 0], stroke_color=GREY, stroke_width=2)
        b1 = Rectangle(width=0.25, height=0.30, stroke_color=self.COL_BLACK, fill_color=self.COL_BLACK,
                       fill_opacity=1).move_to([-0.38, -0.10, 0])
        b2 = Rectangle(width=0.25, height=0.45, stroke_color=self.COL_BLACK, fill_color=self.COL_BLACK,
                       fill_opacity=1).move_to([0.00, 0.02, 0])
        b3 = Rectangle(width=0.25, height=0.22, stroke_color=self.COL_BLACK, fill_color=self.COL_BLACK,
                       fill_opacity=1).move_to([0.38, -0.14, 0])
        e1 = Line([-0.38, 0.05, 0], [-0.38, 0.18, 0], stroke_color=GREY, stroke_width=2)
        e1c = Line([-0.48, 0.18, 0], [-0.28, 0.18, 0], stroke_color=GREY, stroke_width=2)
        e2 = Line([0.00, 0.20, 0], [0.00, 0.35, 0], stroke_color=GREY, stroke_width=2)
        e2c = Line([-0.10, 0.35, 0], [0.10, 0.35, 0], stroke_color=GREY, stroke_width=2)
        e3 = Line([0.38, -0.02, 0], [0.38, 0.10, 0], stroke_color=GREY, stroke_width=2)
        e3c = Line([0.28, 0.10, 0], [0.48, 0.10, 0], stroke_color=GREY, stroke_width=2)
        g.add(base, b1, b2, b3, e1, e1c, e2, e2c, e3, e3c)
        return g

    # save: folder + page slide-in
    def _save_folder_icon(self):
        body = Rectangle(width=1.2, height=0.60, stroke_color=GREY, stroke_width=2,
                         fill_color="#f5f5f5", fill_opacity=1).shift(DOWN * 0.05)
        flap = VGroup(
            Rectangle(width=0.46, height=0.18, stroke_color=GREY, stroke_width=2,
                      fill_color="#ededed", fill_opacity=1).move_to(body.get_top()+LEFT*0.28+DOWN*0.09),
            Rectangle(width=0.74, height=0.18, stroke_color=GREY, stroke_width=2,
                      fill_color="#eaeaea", fill_opacity=1).move_to(body.get_top()+RIGHT*0.18+DOWN*0.09)
        )
        page = Rectangle(width=0.6, height=0.40, stroke_color=GREY, stroke_width=2,
                         fill_color=WHITE, fill_opacity=1).move_to(body.get_top()+UP*0.05)
        g = VGroup(body, flap, page); g.body, g.page = body, page
        return g

    def _save_folder_anim(self, icon):
        page = icon.page
        dest = icon.body.get_center() + UP * 0.10
        self.play(page.animate.move_to(dest), run_time=0.36, rate_func=rate_functions.ease_in_out_quart)
        self.play(Flash(icon.body, flash_radius=0.55, line_length=0.2), run_time=0.16)
