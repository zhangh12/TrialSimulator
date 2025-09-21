# trial_flow.py — TrialFlowV5 (neutral listener, no bubble tail, code-file custom icon, no top title)
# Manim Community Edition >= 0.19

from manim import *
import numpy as np
import random
from manim import config, rate_functions

config.disable_caching = True
MIN_FRAMES = 2  # enforce >=2 frames per segment so updaters advance


class TrialFlowV5(MovingCameraScene):
    """
    • Single-lane flow; R-gate scan → per-arm E-gate close-ups; milestones pause lane but keep stage visible.
    • Arm A: 4 gates [binary, continuous, time-to-event, longitudinal], dwell = 1×T
      Arm B: 2 gates [delayed effect (KM step, overlap→split), PFS/OS (dual calendars)], dwell = 2×T
      Arm C: 1 gate [custom (code file)], dwell = 4×T
    • Listener is a neutral figure; milestone check mark color unified to FLAG_COLOR.
    • Milestone bubble has NO bottom tail (clean rounded rectangle only).
    • No persistent title at the top of the screen.
    """

    # palette
    COL_A = "#377eb8"      # Dose A
    COL_B = "#ff7f00"      # Dose B
    COL_C = "#4daf4a"      # Dose C
    COL_GRAY = "#bdbdbd"
    COL_BLACK = "#111111"
    FLAG_COLOR = "#E91E63"  # independent magenta for milestone flag and check mark

    def __init__(self, **kwargs):
        super().__init__(**kwargs)
        # preview knobs
        self.preview = True             # set False for final render
        self.preview_level = 1
        self.time_scale = 0.55          # set 1.0 for final render
        self.flow_speed = 1.00
        self.flow_running = True
        self.interval_scale = 4.0       # global linger scaler

        # R-gate events
        self.force_next_arm = None
        self.last_cross_event = None    # {"id": int, "arm": "A/B/C", "person": Mobject}
        self.event_id = 0
        self.hidden_stage = None

        # E-gate centered layout
        self.egate_y = 0.0
        self.ep_label_offset = 0.50

        # lane params
        self.lane_spacing = 0.56
        self.lane_cycle = None  # set in construct

    # helpers
    def I(self, t): return t * self.interval_scale
    def is_fast(self, level=1): return self.preview and self.preview_level >= level

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
        # removed persistent title at top
        self.add(baseline)

        # R-gate
        r_gate, lamps = self._build_random_gate(self.x_rg, self.y0)
        self.r_gate_lamps = lamps
        self.add(r_gate)

        # lane (fixed spacing; uniform speed; start with everyone before R-gate)
        SPACING = 0.56
        self.lane_spacing = SPACING
        N = 26 if self.is_fast(1) else 34
        LANE_CYCLE = SPACING * N
        self.lane_cycle = LANE_CYCLE
        self.lane_offset = ValueTracker(0.0)
        self.persons = VGroup(*[self._make_person(self.COL_GRAY).scale(0.95) for _ in range(N)])
        base_start = self.x_rg - N * SPACING - 0.6
        x_anchors = [base_start + i * SPACING for i in range(N)]

        def lane_updater(group, dt):
            # uniform speed by frame; pause when milestones or cutaways
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

        # calm start; long wait happens just before first R-cutaway
        self.wait(self.I(0.8))

        arms_seq = ["A", "B", "C"]
        color_of = {"A": self.COL_A, "B": self.COL_B, "C": self.COL_C}

        for idx, arm in enumerate(arms_seq):
            # wait until this arm reaches R-gate
            if idx == 0:
                self._wait_for_arm_at_gate(arm, min_gap=self.I(0.6))
                self.wait(self.I(2.2))
            else:
                self._wait_for_arm_at_gate(arm, min_gap=self.I(1.8))

            # R-gate cutaway; keep stage hidden to chain into E-gate
            self._rg_cutaway(color_of[arm], scan_rt=1.1, dwell_rt=self.I(0.70), restore_stage=False)

            # E-gate close-up by arm
            if arm == "A":
                plan = [
                    {"x": -3.0, "kind": "binary", "label": "binary"},
                    {"x": -1.0, "kind": "gauss",  "label": "continuous"},
                    {"x":  1.0, "kind": "tte",    "label": "time-to-event"},
                    {"x":  3.0, "kind": "spark",  "label": "longitudinal"},
                ]
                self._zoom_endpoints_for_arm(f"Dose {arm}", color_of[arm], pre_hidden=True,
                                             gate_plan=plan, dwell_factor=1.0, label_over_each=False)
            elif arm == "B":
                plan = [
                    {"x": -1.0, "kind": "delayed", "label": "delayed effect"},
                    {"x":  1.0, "kind": "pfsos",   "label": "PFS/OS"},
                ]
                self._zoom_endpoints_for_arm(f"Dose {arm}", color_of[arm], pre_hidden=True,
                                             gate_plan=plan, dwell_factor=2.0, label_over_each=True)
            else:  # "C"
                plan = [{"x": 0.0, "kind": "custom", "label": "custom"}]
                self._zoom_endpoints_for_arm(f"Dose {arm}", color_of[arm], pre_hidden=True,
                                             gate_plan=plan, dwell_factor=4.0, label_over_each=True)

        # Milestones
        listener1 = self._spawn_listener(target_x=self.x_l1, rt=self.I(1.6))
        self._milestone_bubble_sequential(anchor_x=self.x_l1, ms_index=1, drop_C=True)
        anchor1 = self._listener_to_anchor(listener1, color=self.COL_A); self.add(anchor1)

        self.wait(self.I(1.6))

        listener2 = self._spawn_listener(target_x=self.x_l2, rt=self.I(1.6))
        self._milestone_bubble_sequential(anchor_x=self.x_l2, ms_index=2, drop_C=False)
        anchor2 = self._listener_to_anchor(listener2, color=self.COL_B); self.add(anchor2)

        self.wait(self.I(0.9))
        # removed 'title' from fadeout list
        self.play(FadeOut(VGroup(self.persons, r_gate, anchor1, anchor2, baseline), run_time=0.8))

    # ===== wait for arm at R-gate ==========================================================
    def _wait_for_arm_at_gate(self, arm, min_gap=0.8):
        prev_id = self.event_id
        self.force_next_arm = arm
        self.flow_running = True
        while True:
            self.wait(0.02)
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
        return np.array([base_x + offsets_x[idx % 4], chest_y, 0.0])

    # ======================================================================
    # R-gate & cutaway
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
        """Center-stage R-gate: hide main lane, show scan & recolor, then restore stage optionally."""
        self.flow_running = False
        stage = Group(*[m for m in self.mobjects if m is not self.camera.frame])
        self.play(FadeOut(stage, run_time=0.2))

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

        if restore_stage:
            self.play(FadeIn(stage, run_time=0.2))
            self.flow_running = True

    # ======================================================================
    # Endpoint icons
    # ======================================================================
    def _endpoint_icon(self, kind):
        if kind == "binary":
            return Text("+/-", weight=BOLD, color=WHITE).scale(0.7)

        if kind == "gauss":
            # First-quadrant Gaussian with light axes (x-axis slightly lower)
            x0, y0 = -0.30, -0.20
            x1, y1 = 0.30, 0.18
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
            return self._calendar_base()

        if kind == "spark":
            pts = [LEFT*0.30+DOWN*0.08, LEFT*0.15+UP*0.10, ORIGIN+DOWN*0.02,
                   RIGHT*0.15+UP*0.12, RIGHT*0.30+DOWN*0.05]
            return VMobject(stroke_color=WHITE, stroke_width=5).set_points_as_corners(pts)

        if kind == "delayed":
            # KM step curves: identical pre-mid path → split at mid; solid (lower) steeper than dashed (upper).
            x0, x1 = -0.42, 0.42
            mid = (x0 + x1) / 2.0
            y_start = 0.16
            shared = [
                np.array([x0,         y_start,        0]),
                np.array([x0+0.14,    y_start,        0]),
                np.array([x0+0.14,    y_start-0.02,   0]),
                np.array([x0+0.28,    y_start-0.02,   0]),
                np.array([x0+0.28,    y_start-0.04,   0]),
                np.array([mid,        y_start-0.04,   0]),
            ]
            solid_tail = [
                np.array([mid,        y_start-0.04,   0]),
                np.array([mid+0.10,   y_start-0.12,   0]),
                np.array([mid+0.22,   y_start-0.17,   0]),
                np.array([x1,         y_start-0.19,   0]),
            ]
            dashed_tail = [
                np.array([mid,        y_start-0.04,   0]),
                np.array([mid+0.10,   y_start-0.06,   0]),
                np.array([mid+0.22,   y_start-0.07,   0]),
                np.array([x1,         y_start-0.08,   0]),
            ]
            solid_pts = shared + solid_tail
            dashed_pts = shared + dashed_tail
            solid = VMobject(stroke_color=WHITE, stroke_width=2.0).set_points_as_corners(solid_pts)
            dashed_curve = VMobject(stroke_color=WHITE, stroke_width=2.0).set_points_as_corners(dashed_pts)
            dashed = DashedVMobject(dashed_curve, num_dashes=24, dashed_ratio=0.55)
            x_axis = Line([x0, -0.22, 0], [x1, -0.22, 0], stroke_color=GREY_D, stroke_width=2)
            return VGroup(x_axis, solid, dashed)

        if kind == "pfsos":
            # dual calendars with headers
            cal_pfs = self._calendar_base()
            cal_os  = self._calendar_base()
            hdr_p = cal_pfs[1]; hdr_o = cal_os[1]
            tag_p = Text("PFS", weight=BOLD, color=BLACK).scale(0.28).move_to(hdr_p.get_center())
            tag_o = Text("OS",  weight=BOLD, color=BLACK).scale(0.28).move_to(hdr_o.get_center())
            cal_pfs = VGroup(cal_pfs, tag_p)
            cal_os  = VGroup(cal_os, tag_o)
            g = VGroup(cal_pfs, cal_os).arrange(RIGHT, buff=0.16).scale(0.92)
            return g

        if kind == "custom":
            # Code file: page with folded corner + "</>" + code lines
            page = RoundedRectangle(corner_radius=0.05, width=0.95, height=0.60,
                                    stroke_color=WHITE, stroke_width=3,
                                    fill_color=WHITE, fill_opacity=1.0)
            fold = Polygon(
                page.get_top()+RIGHT*0.30+DOWN*0.02,
                page.get_top()+RIGHT*0.12+DOWN*0.02,
                page.get_top()+RIGHT*0.30+DOWN*0.20,
                stroke_color=GREY_B, fill_color="#eaeaea", stroke_width=2.0
            )
            code = Text("</>", weight=BOLD, color=BLACK).scale(0.50)
            code.move_to(page.get_center()+UP*0.08)

            l1 = Line(page.get_left()+RIGHT*0.12+DOWN*0.04,
                      page.get_left()+RIGHT*0.42+DOWN*0.04,
                      stroke_color=GREY_B, stroke_width=2.0)
            l2 = Line(page.get_left()+RIGHT*0.12+DOWN*0.12,
                      page.get_left()+RIGHT*0.32+DOWN*0.12,
                      stroke_color=GREY_B, stroke_width=2.0)
            l3 = Line(page.get_left()+RIGHT*0.12+DOWN*0.20,
                      page.get_left()+RIGHT*0.38+DOWN*0.20,
                      stroke_color=GREY_B, stroke_width=2.0)

            return VGroup(page, fold, code, l1, l2, l3)

        return Square(0.2, color=WHITE, stroke_width=5)

    def _calendar_base(self):
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

    def _build_endpoint_gate(self, x, y, kind):
        frame = RoundedRectangle(corner_radius=0.2, width=1.3, height=1.9,
                                 stroke_color=GREY, stroke_width=2, fill_opacity=0).move_to([x, y, 0])
        plaque = RoundedRectangle(corner_radius=0.14, width=0.95, height=0.68,
                                  fill_color=BLACK, fill_opacity=1.0, stroke_width=0)\
            .move_to(frame.get_top() + DOWN * 0.25)
        ico = self._endpoint_icon(kind).scale(0.60).move_to(plaque.get_center())
        return VGroup(frame, plaque, ico)

    def _zoom_endpoints_for_arm(self, label, color, pre_hidden=False,
                                gate_plan=None, dwell_factor=1.0, label_over_each=False):
        """Hide stage if not pre-hidden; zoom to gate rectangle; show gates and demo person with badges at each stop."""
        if not pre_hidden:
            self.flow_running = False
            stage_hidden_now = Group(*[m for m in self.mobjects if m is not self.camera.frame])
            self.play(FadeOut(stage_hidden_now, run_time=0.2))
            self.hidden_stage = stage_hidden_now

        self.camera.frame.save_state()
        target_rect = Rectangle(width=8.8, height=4.2, stroke_opacity=0).move_to(ORIGIN)
        self.play(self.camera.frame.animate.move_to(target_rect).set(width=9.2), run_time=0.35)

        y = self.egate_y
        lane = Line(LEFT * 4.4 + UP * y, RIGHT * 4.4 + UP * y, stroke_opacity=0.15)
        self.add(lane)

        if gate_plan is None:
            gate_plan = [
                {"x": -3.0, "kind": "binary", "label": "binary"},
                {"x": -1.0, "kind": "gauss",  "label": "continuous"},
                {"x":  1.0, "kind": "tte",    "label": "time-to-event"},
                {"x":  3.0, "kind": "spark",  "label": "longitudinal"},
            ]

        xs = [d["x"] for d in gate_plan]
        kinds = [d["kind"] for d in gate_plan]
        labels = [d["label"] for d in gate_plan]

        gates = VGroup(*[self._build_endpoint_gate(x, y, k) for x, k in zip(xs, kinds)])
        self.add(gates)

        # Dose label below gates
        gates_bottom = min(g.get_bottom()[1] for g in gates)
        tag = Text(label, weight=BOLD, color=color).scale(0.5)
        tag.move_to([0, gates_bottom - 0.40, 0])
        self.add(tag)

        def show_ep_label_midpair(txt, arm_color):
            # above the middle pair for 4 gates; above the first for 1–2 gates
            if len(gates) >= 3:
                mid_top_y = max(gates[len(gates)//2 - 0].get_top()[1], gates[len(gates)//2 - 1].get_top()[1]) \
                    if len(gates) >= 4 else gates[0].get_top()[1]
                center_x = (xs[len(xs)//2 - 1] + xs[len(xs)//2]) / 2 if len(xs) >= 4 else xs[0]
            else:
                mid_top_y = gates[0].get_top()[1]; center_x = xs[0]
            lbl = Text(txt, weight=BOLD, color=WHITE).scale(0.56)
            pad = 0.18
            L = lbl.width + 2 * pad
            underline = Line([0 - L/2, 0, 0], [0 + L/2, 0, 0], stroke_width=6, color=arm_color, stroke_opacity=0.9)
            group = VGroup(lbl, underline).arrange(DOWN, buff=0.06)
            group.move_to([center_x, mid_top_y + self.ep_label_offset, 0])
            self.play(FadeIn(group, run_time=0.18))
            return group

        def show_ep_label_above_gate(idx, txt, arm_color):
            g = gates[idx]; top_y = g.get_top()[1]; center_x = xs[idx]
            lbl = Text(txt, weight=BOLD, color=WHITE).scale(0.56)
            pad = 0.18; L = lbl.width + 2 * pad
            underline = Line([0 - L/2, 0, 0], [0 + L/2, 0, 0], stroke_width=6, color=arm_color, stroke_opacity=0.9)
            group = VGroup(lbl, underline).arrange(DOWN, buff=0.06)
            group.move_to([center_x, top_y + self.ep_label_offset, 0])
            self.play(FadeIn(group, run_time=0.18))
            return group

        demo = self._make_person(color).scale(1.05).move_to([xs[0] - 1.2, y, 0])
        self.add(demo)

        def flash_badge(idx):
            badge = Circle(radius=0.11, fill_color=WHITE, fill_opacity=1, stroke_width=0)
            badge.move_to(self._badge_anchor_on_chest_idx(demo, idx))
            self.add(badge)
            self.play(FadeIn(badge, scale=0.85, run_time=0.12))
            self.play(Flash(badge, flash_radius=0.25, line_length=0.10), run_time=0.12)
            self.play(FadeOut(badge, run_time=0.14))

        base_T = self.I(0.40) * dwell_factor

        for i, x in enumerate(xs):
            self.play(demo.animate.move_to([x, y, 0]), run_time=0.42, rate_func=linear)
            self.wait(base_T * 0.40)
            if label_over_each:
                lbl_grp = show_ep_label_above_gate(i, labels[i], color)
            else:
                lbl_grp = show_ep_label_midpair(labels[i], color)
            flash_badge(i)
            self.wait(base_T * 0.60)
            self.play(FadeOut(lbl_grp, run_time=0.18))

        self.play(FadeOut(VGroup(lane, tag, gates, demo), run_time=0.25))
        self.play(self.camera.frame.animate.restore(), run_time=0.35)
        if self.hidden_stage is not None:
            self.play(FadeIn(self.hidden_stage, run_time=0.2))
            self.hidden_stage = None
        self.flow_running = True

    # ======================================================================
    # Listener (neutral) & milestones
    # ======================================================================
    def _build_listener(self):
        """Neutral stick/figure listener: head + torso + simple arm line."""
        head = Circle(
            radius=0.10,
            fill_color=WHITE, fill_opacity=1,
            stroke_color=self.COL_BLACK, stroke_width=2
        )
        body = RoundedRectangle(
            width=0.36, height=0.52, corner_radius=0.10,
            fill_color=WHITE, fill_opacity=1,
            stroke_color=self.COL_BLACK, stroke_width=2
        )
        body.next_to(head, DOWN, buff=0.05)
        arm = Line(
            body.get_top()+DOWN*0.18+LEFT*0.15,
            body.get_top()+DOWN*0.05+RIGHT*0.18,
            stroke_color=self.COL_BLACK, stroke_width=2
        )
        return VGroup(head, body, arm)

    def _spawn_listener(self, target_x, rt=1.6):
        """Enter from left to target position with a smooth ease; no extra props."""
        listener = self._build_listener().move_to([self.x_min - 1.2, self.y0 + 0.9, 0]).scale(1.0)
        self.add(listener)
        self.play(
            listener.animate.move_to([target_x, self.y0 + 0.9, 0]),
            run_time=rt, rate_func=rate_functions.smooth
        )
        return listener

    def _listener_to_anchor(self, listener, color=BLUE):
        """Transform the figure into a vertical marker with a colored node,
        a FLAG_COLOR check, and a FLAG_COLOR flag; then attach lane-follow updater."""
        x = listener.get_center()[0]
        line = Line([x, self.y0, 0], [x, self.y0 + 0.85, 0], stroke_color=color, stroke_width=3)
        node = Circle(radius=0.12, fill_color=color, fill_opacity=1, stroke_width=0).move_to([x, self.y0 + 0.85, 0])

        # unified check color = FLAG_COLOR
        check = VGroup(
            Line(
                node.get_center()+LEFT*0.07+DOWN*0.02,
                node.get_center()+ORIGIN+DOWN*0.08,
                stroke_width=3, color=self.FLAG_COLOR
            ),
            Line(
                node.get_center()+ORIGIN+DOWN*0.08,
                node.get_center()+RIGHT*0.09+UP*0.06,
                stroke_width=3, color=self.FLAG_COLOR
            )
        )
        plate = VGroup(node, check)

        # flag also uses FLAG_COLOR
        pole = Line(
            node.get_center()+RIGHT*0.12+DOWN*0.10,
            node.get_center()+RIGHT*0.12+UP*0.12,
            stroke_color=self.FLAG_COLOR, stroke_width=2.5
        )
        flag = Polygon(
            pole.get_end(),
            pole.get_end()+RIGHT*0.22+DOWN*0.08,
            pole.get_end()+RIGHT*0.22+UP*0.08,
            stroke_color=self.FLAG_COLOR, fill_color=self.FLAG_COLOR, fill_opacity=1, stroke_width=2
        )
        flag_group = VGroup(pole, flag)

        anchor = VGroup(line, plate, flag_group)

        self.play(Transform(listener, node), run_time=0.30)
        self.play(Create(line, run_time=0.32))
        self.play(FadeIn(VGroup(check, flag_group), run_time=0.22))
        self.remove(listener)

        # follow the lane horizontally (milestone marker drifts with the flow)
        start_off = self.lane_offset.get_value()
        base_y = anchor.get_center()[1]
        base_x = x
        def follow_updater(mob, dt):
            dx = self.lane_offset.get_value() - start_off
            mob.move_to([base_x + dx, base_y, 0])
        anchor.add_updater(follow_updater)

        return anchor

    def _milestone_bubble_sequential(self, anchor_x, ms_index, drop_C):
        """Pause lane; zoom to bubble; show steps one-by-one with longer dwell; optionally drop arm C; save results.
           Bubble is a rounded rectangle WITHOUT a triangular tail."""
        prev = self.flow_running; self.flow_running = False

        self.camera.frame.save_state()
        focus = Rectangle(width=8.0, height=4.5, stroke_opacity=0).move_to([anchor_x, self.y0 + 1.6, 0])
        self.play(self.camera.frame.animate.move_to(focus).set(width=8.6), run_time=0.38)

        bubble = RoundedRectangle(corner_radius=0.25, width=6.8, height=3.0,
                                  stroke_color=GREY, fill_color=WHITE, fill_opacity=1.0)\
            .move_to([anchor_x, self.y0 + 1.6, 0])
        title = Text(f"Milestone {ms_index}", weight=BOLD).scale(0.46)\
            .next_to(bubble.get_top(), DOWN, buff=0.18).move_to([anchor_x, bubble.get_top()[1]-0.35, 0])
        # NO tail polygon
        panel = VGroup(bubble, title)
        self.play(FadeIn(panel, run_time=0.22))

        center = np.array([anchor_x, bubble.get_center()[1] - 0.10, 0])

        def show_step_centered(label_txt, icon_mobj, linger=1.35, keep=False):
            label = Text(label_txt, weight=MEDIUM, color="#222").scale(0.46)
            row = VGroup(icon_mobj, label).arrange(RIGHT, buff=0.25).move_to(center)
            self.play(FadeIn(row, run_time=0.20))
            self.wait(self.I(linger))
            if not keep: self.play(FadeOut(row, run_time=0.18))
            return row

        # 1) Data snapshot — camera → flash (camera only) → transform to table
        cam = self._camera_icon()
        row = show_step_centered("Data snapshot", cam, linger=0.90, keep=True)
        cam_icon = row[0]
        self.play(Flash(cam_icon, flash_radius=0.45, line_length=0.16), run_time=0.18)  # text does not flash
        tbl = self._table_icon().scale(0.92).move_to(cam_icon.get_center())
        self.play(Transform(cam_icon, tbl), run_time=0.28)
        self.wait(self.I(0.55))
        self.play(FadeOut(row, run_time=0.18))

        # 2) Stat analysis
        stat = self._bars_icon().scale(0.92)
        show_step_centered("Stat analysis", stat, linger=1.40)

        # 3) Decision (MS1 only)
        if drop_C:
            dots = VGroup(
                Circle(radius=0.22, fill_color=self.COL_A, fill_opacity=1, stroke_width=0),
                Circle(radius=0.22, fill_color=self.COL_B, fill_opacity=1, stroke_width=0),
                Circle(radius=0.22, fill_color=self.COL_C, fill_opacity=1, stroke_width=0),
            ).arrange(RIGHT, buff=0.40)
            row = show_step_centered("Decision", dots, linger=0.90, keep=True)
            C = dots.submobjects[2]
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

        # 4) Save results — page slides inside folder (bottom edge disappears first)
        folder = self._save_folder_icon().scale(1.0)
        row = show_step_centered("Save results", folder, linger=1.45, keep=True)
        if hasattr(folder, "page"):
            self._save_folder_anim(folder)
        self.wait(self.I(0.30))
        self.play(FadeOut(row, run_time=0.18))

        self.play(FadeOut(panel, run_time=0.20))
        self.play(self.camera.frame.animate.restore(), run_time=0.32)
        self.flow_running = prev

    # ======================================================================
    # Small icons
    # ======================================================================
    def _camera_icon(self):
        body = Rectangle(width=0.62, height=0.36, fill_color=BLACK, fill_opacity=1, stroke_width=0)
        lens = Circle(radius=0.11, color=WHITE, fill_opacity=0, stroke_width=3)
        return VGroup(body, lens)

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

    # save: folder with back plate and front lip; page goes BEHIND back so bottom disappears first
    def _save_folder_icon(self):
        back = Rectangle(width=1.2, height=0.60, stroke_color=GREY, stroke_width=2,
                         fill_color="#f5f5f5", fill_opacity=1).shift(DOWN * 0.05)
        lip = Rectangle(width=1.20, height=0.20, stroke_color=GREY, stroke_width=2,
                        fill_color="#f5f5f5", fill_opacity=1).move_to(back.get_top()+DOWN*0.10)
        flap = VGroup(
            Rectangle(width=0.46, height=0.18, stroke_color=GREY, stroke_width=2,
                      fill_color="#ededed", fill_opacity=1).move_to(back.get_top()+LEFT*0.28+DOWN*0.09),
            Rectangle(width=0.74, height=0.18, stroke_color=GREY, stroke_width=2,
                      fill_color="#eaeaea", fill_opacity=1).move_to(back.get_top()+RIGHT*0.18+DOWN*0.09)
        )
        page = Rectangle(width=0.6, height=0.40, stroke_color=GREY, stroke_width=2,
                         fill_color=WHITE, fill_opacity=1).move_to(back.get_top()+UP*0.05)

        # z-order: page (6 initially) > lip(7) > flap(8); back(5) — we'll move page BEHIND back later
        back.set_z_index(5); page.set_z_index(6); lip.set_z_index(7); flap.set_z_index(8)

        g = VGroup(back, lip, flap, page)
        g.back, g.lip, g.flap, g.page = back, lip, flap, page
        return g

    def _save_folder_anim(self, icon):
        page = icon.page
        back = icon.back
        lip  = icon.lip

        # phase 1: align at slot entrance (still above back)
        entrance = lip.get_top() + DOWN*0.02
        self.play(page.animate.move_to(entrance), run_time=0.22, rate_func=rate_functions.ease_in_out_sine)

        # phase 2: put page BEHIND the back plate so bottom disappears first, then slide inside
        page.set_z_index(4)  # strictly below back(5) and lip(7)
        inside = back.get_center() + DOWN*0.02
        self.play(page.animate.move_to(inside), run_time=0.26, rate_func=rate_functions.ease_in_out_quart)

        # small shimmer on back plate
        self.play(Flash(back, flash_radius=0.55, line_length=0.2), run_time=0.16)
