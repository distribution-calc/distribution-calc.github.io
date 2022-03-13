function cap_first_letter(e) {
  return e[0].toUpperCase() + e.slice(1).toLowerCase();
}
function two_values() {
  var e = document.getElementById("check").value;
  "x2" == e || "p2" == e
    ? display_blocks("value2", "block")
    : display_blocks("value2", "none"),
    "p" == e || "p2" == e
      ? (set_variable("val1", "p<sub>1</sub>"),
        set_variable("val2", "p<sub>2</sub>"))
      : (set_variable("val1", "x<sub>1</sub> percentile"),
        set_variable("val2", "x<sub>2</sub> percentile"));
}
function change_distribution() {
  GEBI("normal").innerHTML = "";
  var e = get_variable("distribution1"),
    r = GEBI("df1");
  "Normal" == e
    ? (display_blocks("x1_r,sigma1_r", "block"),
      display_blocks(
        "df_r,df2_r,numerator,p_r,n_r,lambda_r,scale_r,shape_r",
        "none"
      ))
    : "binomial" == e
    ? (display_blocks("p_r,n_r", "block"),
      display_blocks(
        "x1_r,sigma1_r,df_r,df2_r,numerator,lambda_r,scale_r,shape_r",
        "none"
      ))
    : "T" == e || "Chi-Square" == e
    ? (display_blocks("df_r", "block"),
      display_blocks(
        "x1_r,sigma1_r,df2_r,numerator,p_r,n_r,lambda_r,scale_r,shape_r",
        "none"
      ),
      (r.innerHTML = "Degrees of freedom"))
    : "poisson" == e
    ? (display_blocks("df_r,lambda_r", "block"),
      display_blocks(
        "x1_r,sigma1_r,df_r,df2_r,numerator,p_r,n_r,scale_r,shape_r",
        "none"
      ))
    : "weibull" == e
    ? (display_blocks("df_r,scale_r,shape_r", "block"),
      display_blocks(
        "x1_r,sigma1_r,df_r,df2_r,numerator,p_r,n_r,lambda_r",
        "none"
      ))
    : "exponential" == e
    ? (display_blocks("df_r,scale_r", "block"),
      display_blocks(
        "x1_r,sigma1_r,df_r,df2_r,numerator,p_r,n_r,lambda_r,shape_r",
        "none"
      ),
      (document.getElementById("shape").value = 1))
    : "F" == e &&
      (display_blocks("numerator", "inline"),
      display_blocks("df_r,df2_r", "block"),
      display_blocks("x1_r,sigma1_r,p_r,n_r,lambda_r,scale_r,shape_r", "none"),
      (r.innerHTML = "Degrees of freedom - numerator"));
}
function calculate(e, r) {
  var a;
  display_blocks("results", ""), "exponential" == e && (e = "weibull");
  var o,
    i,
    l,
    t,
    n,
    s,
    d,
    b,
    u,
    p,
    c,
    m,
    _,
    x,
    g,
    f,
    h,
    v,
    k,
    y = "",
    X = 0,
    w = performance.now(),
    P = get_var("digits");
  u = null != P ? 1 * P : 6;
  var E,
    S,
    C,
    D,
    T,
    F,
    q,
    R,
    L = "",
    M = "",
    N = "",
    A = "R code should be:<br>";
  0 == r && (r = 1e-5);
  var I,
    U = 1 * get_var("xp"),
    z = U,
    G = 1 * get_var("xp2"),
    B = GEBI("messageC"),
    O = get_var("check"),
    V = "",
    H = findO(ODistribution, "type", e),
    Z = ODistribution[H],
    W = "<br>";
  if (
    (("Normal" != e && "Normal2" != e) ||
      (E =
        "<span class='d-inline-block' tabindex='0' data-toggle='tooltip' data-html='true' title='Z is the value of the standard normal distribution with &mu;=0 and &sigma;=1. Z=(x-&mu;)/&sigma;.'><span class='text-info'>z<sub>1</sub></span></span>"),
    "x2" == O && (W = " - <br>"),
    "Normal" == e || "Normal2" == e
      ? ((a = {
          mean: (T = l = 1 * get_var("x1")),
          std: (F = t = 1 * get_var("sigma1")),
        }),
        (V += check_number("&mu;", l)),
        (V += check_number("&sigma;", t)),
        (M = A),
        "x" == O || "x2" == O
          ? (N = "pnorm(q=")
          : ("p" != O && "p2" != O) || (N = "qnorm(p="),
        (h =
          N +
          G +
          ", mean=" +
          l +
          ", sd=" +
          t +
          ", lower.tail=TRUE, log.p = FALSE)" +
          W),
        "x2" == O && (M += h),
        (M +=
          N +
          U +
          ", mean=" +
          l +
          ", sd=" +
          t +
          ", lower.tail=TRUE, log.p = FALSE)<br>"),
        "p2" == O && (M += h))
      : "weibull" == e
      ? ((a = {
          scale: (n = 1 * get_var("scale")),
          shape: (s = 1 * get_var("shape")),
        }),
        (M = A),
        "x" == O || "x2" == O
          ? (N = "pweibull(q=")
          : ("p" != O && "p2" != O) || (N = "qweibull(p="),
        (h =
          N +
          G +
          ", shape=" +
          s +
          ", scale=" +
          n +
          ", lower.tail=TRUE, log.p = FALSE)" +
          W),
        "x2" == O && (M += h),
        (M +=
          N +
          U +
          ", shape=" +
          s +
          ", scale=" +
          n +
          ", lower.tail=TRUE, log.p = FALSE)<br>"),
        "p2" == O && (M += h))
      : "binomial" == e
      ? ((o = 1 * get_var("n")),
        (prob0 = 1 * get_variable("p")),
        (a = { n: o, p: prob0 }),
        (V += check_number("n", o)),
        (V += check_number("p", prob0)),
        (V += checkRange("p", i, 0, 1)),
        (M = A),
        "x" == O || "x2" == O
          ? (N = "pbinom(q=")
          : ("p" != O && "p2" != O) || (N = "qbinom(p="),
        (h =
          N +
          G +
          ", size=" +
          o +
          ", prob=" +
          prob0 +
          ", lower.tail=TRUE, log.p = FALSE)" +
          W),
        "x2" == O && ((M += h), (z = U - 1 < 0 ? 0 : U - 1)),
        (M +=
          N +
          z +
          ", size=" +
          o +
          ", prob=" +
          prob0 +
          ", lower.tail=TRUE, log.p = FALSE)<br>"),
        "p2" == O && (M += h))
      : "poisson" == e
      ? ((lambda = 1 * get_var("lambda")),
        (T = lambda),
        (F = 1),
        (a = { lambda: lambda }),
        (V += check_number("lambda", lambda)),
        (V += checkRange("lambda", lambda, 0, 1 / 0)),
        (V +=
          "x" == O || "x2" == O
            ? check_number("Value", U)
            : checkRange("Value", U, 0, 1)),
        (M = A),
        "x" == O || "x2" == O
          ? (N = "ppois(q=")
          : ("p" != O && "p2" != O) || (N = "qpois(p="),
        (h =
          N +
          G +
          ", lambda=" +
          lambda +
          ", lower.tail=TRUE, log.p = FALSE)" +
          W),
        "x2" == O && ((M += h), (z = U - 1 < 0 ? 0 : U - 1)),
        (M +=
          N +
          z +
          ", lambda=" +
          lambda +
          ", lower.tail=TRUE, log.p = FALSE)<br>"),
        "p2" == O && (M += h))
      : "Chi-Square" == e || "T" == e
      ? ((v = "Chi-Square" == e ? "chisq" : "t"),
        (a = { df: (T = 1 * get_var("df")) }),
        (V += checkRange("df", T, 0, 1 / 0)),
        (M = A),
        "x" == O || "x2" == O
          ? (N = "p" + v + "(q=")
          : ("p" != O && "p2" != O) || (N = "q" + v + "(p="),
        (h =
          N + G + ", df=" + T + ", ncp=0, lower.tail=TRUE, log.p = FALSE)" + W),
        "x2" == O && (M += h),
        (M +=
          N + U + ", df=" + T + ", ncp=0, lower.tail=TRUE, log.p = FALSE)<br>"),
        "p2" == O && (M += h))
      : "F" == e &&
        ((df1 = 1 * get_var("df")),
        (F = 1 * get_var("df2")),
        (V += check_number("n", df1)),
        (V += checkInt("n", df1)),
        (V += checkRange("n", df1, 0, 1 / 0)),
        (V += check_number("m", F)),
        (V += checkInt("m", F)),
        (V += checkRange("m", F, 0, 1 / 0)),
        (a = { df1: df1, df2: F }),
        (M = A),
        "x" == O || "x2" == O
          ? (N = "pf(q=")
          : ("p" != O && "p2" != O) || (N = "qf(p="),
        (h =
          N +
          G +
          ", df1=" +
          df1 +
          ", df2=" +
          F +
          ", ncp=0, lower.tail=TRUE, log.p = FALSE)" +
          W),
        "x2" == O && (M += h),
        (M +=
          N +
          U +
          ", df1=" +
          df1 +
          ", df2=" +
          F +
          ", ncp=0, lower.tail=TRUE, log.p = FALSE)<br>"),
        "p2" == O && (M += h)),
    (V += check_number("x1 or P( X &le; x1 )", U)),
    debug.log("last check"),
    (V += check_number("x2 or P( X &le; x2 )", U)),
    "p" == O && (V += checkRange(" P( X &le; x1) ", U, 0, 1)),
    0 == V.length)
  ) {
    if ("x" == O)
      (L =
        "Normal" == e || "Normal2" == e
          ? ((S = (U - l) / t),
            debug.log("z:" + S),
            E + " = " + xround(S, u) + "<br>")
          : ((S = U), "")),
        Z.discrete &&
          ((b = 0 < U - (d = Math.floor(U))) &&
            (y =
              "<br>Since the " +
              e +
              " distribution is discrete, the probability to get any non-integer value between " +
              d +
              " and " +
              (d + 1) +
              " is zero."),
          (S = U = d)),
        (q = pdf(e, U, a)),
        (i = Distribution(e, U, a)),
        debug.log("p=" + i),
        (L += "P( X &le; " + U + " ) = " + xround(i, u) + ".<br>"),
        Z.discrete &&
          (L += "P( X < " + U + " ) = " + xround(i - q, u) + ".<br>"),
        (L += "P( X &gt; " + U + " ) = " + xround(1 - i, u) + ".<br>"),
        Z.discrete &&
          (L += "P( X &ge; " + U + " ) = " + xround(1 - (i - q), u) + ".<br>"),
        ("T" == e || ("Normal" == e && 0 == a.mean)) &&
          (L +=
            "P(-" +
            (I = Math.abs(U)) +
            " &le; X &le; " +
            I +
            " ) = " +
            xround(1 - 2 * Math.min(i, 1 - i), u) +
            ".<br>"),
        (m = c = U);
    else if ("x2" == O)
      G < U &&
        (alert("Value₂ must be greater than value₁. I switch the values!"),
        (f = G),
        (G = U),
        (U = f)),
        (L =
          "Normal" == e || "Normal2" == e
            ? ((S = (U - l) / t),
              (C = (G - l) / t),
              E +
                " = " +
                xround(S, u) +
                ", z<sub>2</sub> = " +
                xround(C, u) +
                ".<br>")
            : ((S = U), (C = G), "")),
        debug.log("df:" + T),
        Z.discrete &&
          ((d = Math.floor(U)),
          (floor2 = Math.floor(G)),
          (b = 0 < U - d),
          (non_integer2 = 0 < G - floor2),
          (b || non_integer2) &&
            ((y = "<br>Since the " + e + " distribution is discrete, "),
            b &&
              ((y +=
                "the probability to get any non-integer value between " +
                d +
                " and " +
                (d + 1) +
                " is zero"),
              non_integer2 && (y += ", and ")),
            non_integer2 &&
              (y +=
                "the probability to get any non-integer value between " +
                floor2 +
                " and " +
                (floor2 + 1) +
                " is zero"),
            (y += ".<br>")),
          (S = U = d),
          (C = G = floor2)),
        (q = pdf(e, U, a)),
        (density2 = pdf(e, G, a)),
        (i = Distribution(e, U, a)),
        (D = Distribution(e, G, a)),
        debug.log("p=" + i),
        Z.discrete
          ? ((L +=
              "P( " +
              U +
              "&le; X &le; " +
              G +
              " ) = " +
              xround(D - i + q, u) +
              ".<br>"),
            (L +=
              "P( " +
              U +
              "&lt; X &lt; " +
              G +
              " ) = " +
              xround(D - i - density2, u) +
              ".<br>"))
          : (L +=
              "P( " +
              U +
              "&le; X &le; " +
              G +
              " ) = " +
              xround(D - i, u) +
              ".<br>"),
        (L += "P( X &le; " + U + " ) = " + xround(i, u) + ".<br>"),
        Z.discrete &&
          (L += "P( X < " + U + " ) = " + xround(i - q, u) + ".<br>"),
        (L += "P( X &gt; " + G + " ) = " + xround(1 - D, u) + ".<br>"),
        Z.discrete &&
          (L += "P( X &ge; " + G + " ) = " + xround(1 - (D - q), u) + ".<br>"),
        ("T" == e || ("Normal" == e && 0 == a.mean)) &&
          (L +=
            "P(-" +
            (I = Math.abs(U)) +
            " &le; X &le; " +
            I +
            " ) = " +
            xround(1 - 2 * Math.min(i, 1 - i), u) +
            ".<br>"),
        (m = U),
        (X = G);
    else if ("p" == O) {
      if (
        ((c = InvDistribution(e, U, a)),
        debug.log("z=" + S),
        (S = (c - l) / t),
        Math.min(U, 1 - U),
        (xx_b = InvDistribution(e, (1 - U) / 2, a)),
        (p_c = 0.5 < U ? 2 * U - 1 : 1 - 2 * U),
        (R = Z.discrete ? xround(Distribution(e, c, a), u) : U),
        (q = pdf(e, c, a)),
        Z.discrete
          ? ((L +=
              "Since " +
              e +
              " distribution is a discrete distribution, there is usually no X that meets the exact probability.<br><br>"),
            (m = c))
          : ((m = xround(c, u)),
            (x = xround(xx_b, u)),
            (_ = Math.abs(m)),
            (xxr_b_abs = Math.abs(x))),
        (L += "𝑥₁ = " + m),
        (p = xround(S, u)),
        ("Normal" != e && "Normal2" != e) || (L += ", " + E + " = " + p + "."),
        (L += "<br>P( X &le; " + m + " ) = " + R + ".<br>"),
        Z.discrete)
      ) {
        xround(Distribution(e, c - 1, a), u);
        L +=
          "P( X &le; " +
          (m - 1) +
          " ) = " +
          xround(Distribution(e, c - 1, a), u) +
          "<br>";
      }
      (L += "P( X &gt; " + m + " ) = " + xround(1 - R, u) + ".<br>"),
        ("T" == e || ("Normal" == e && 0 == a.mean)) &&
          ((L +=
            "P(-" +
            _ +
            " &le; X &le; " +
            _ +
            " ) = " +
            xround(p_c, u) +
            ".<br>"),
          (L +=
            "P(-" +
            xxr_b_abs +
            " &le; X &le; " +
            xxr_b_abs +
            " ) = " +
            U +
            ".<br>"));
    } else if ("p2" == O) {
      if (
        (G < U &&
          (alert("Value₂ must be greater than value₁. I switch the values!"),
          (f = G),
          (G = U),
          (U = f)),
        (S = ((c = InvDistribution(e, U, a)) - l) / t),
        (C = ((g = InvDistribution(e, G, a)) - l) / t),
        (D = Z.discrete
          ? ((p1 = xround(Distribution(e, c, a), u)),
            xround(Distribution(e, g, a), u))
          : ((p1 = U), G)),
        (q = GenFunction(e, c, a)),
        (density2 = GenFunction(e, g, a)),
        (X = Z.discrete
          ? ((L +=
              "Since " +
              e +
              " distribution is a discrete distribution, there is usually no X that meets the exact probability.<br><br>"),
            (m = c),
            g)
          : ((m = xround(c, u)), xround(g, u))),
        (L += "X<sub>1</sub> = " + m + ".<br>"),
        (L += "X<sub>2</sub> = " + X + ".<br>"),
        (info1 =
          "<span class='d-inline-block' tabindex='0' data-toggle='tooltip' data-html='true' title='Z is the value of the standard normal distribution with &mu;=0 and &sigma;=1. The tool calculates any normal distrubtion based on Z=(x-&mu;)/&sigma;'><span class='text-info'>more</span></span>"),
        Z.discrete
          ? (L +=
              "P( " +
              (m + 1) +
              "&le; X &le; " +
              X +
              " ) = " +
              D +
              " - " +
              p1 +
              " = " +
              xround(D - p1, u) +
              ".<br>")
          : (L +=
              "P( " +
              m +
              "&le; X &le; " +
              X +
              " ) = " +
              D +
              " - " +
              p1 +
              " = " +
              xround(D - p1, u) +
              ".<br>"),
        (L += "P( X &le; " + m + " ) = " + p1 + ".<br>"),
        (L += "P( X &le; " + X + " ) = " + D + ".<br>"),
        Z.discrete)
      ) {
        xround(Distribution(e, c - 1, a), u);
        (L +=
          "P( X &le; " +
          (m - 1) +
          " ) = " +
          xround(Distribution(e, c - 1, a), u) +
          ".<br>"),
          (L +=
            "P( X &le; " +
            (X - 1) +
            " ) = " +
            xround(Distribution(e, g - 1, a), u) +
            ".<br>");
      }
    } else debug.error("check type should be 'p' or 'x' ");
    "binomial" == e
      ? (M +=
          "dbinom(x=" +
          Math.floor(m) +
          ", size=" +
          o +
          ", prob=" +
          prob0 +
          ",log = FALSE)<br>")
      : "poisson" == e
      ? (M += "dpois(x=" + Math.floor(m) + ", lambda=" + lambda + ")")
      : "Chi-Square" == e || "T" == e
      ? (M += "d" + v + "(x=" + xround(m, u) + ", df=" + T + ", ncp=0)")
      : "Normal" == e
      ? (M += "dnorm (x=" + xround(m, u) + ", mean=" + l + ", sd=" + t + ")")
      : "F" == e
      ? (M +=
          "df(x=" + xround(m, u) + ", df1=" + df1 + ", df2=" + F + ", ncp=0)")
      : "weibull" == e &&
        (M +=
          "dweibull(x=" +
          xround(m, u) +
          ", shape=" +
          s +
          ", scale =" +
          n +
          ")"),
      ("x2" != O && "p2" != O) ||
        ((M += "<br>"),
        "binomial" == e
          ? (M +=
              "dbinom(x=" +
              Math.floor(X) +
              ", size=" +
              o +
              ", prob=" +
              xround(i, u) +
              ",log = FALSE)<br>")
          : "poisson" == e
          ? (M += "dpois(x=" + Math.floor(X) + ", lambda=" + lambda + ")")
          : ("Chi-Square" != e && "T" != e) ||
            (M += "d" + v + "(x=" + xround(X, u) + ", df=" + T + ", ncp=0)")),
      (L +=
        (k = Z.discrete ? "P( X = " : "Probability density( ") + m + " ) = "),
      (L += xround(q, u) + ".<br>"),
      ("x2" != O && "p2" != O) ||
        (L += k + X + " ) = " + xround(density2, u) + ".<br>"),
      Z.discrete && (L += y),
      drawDistributionCalc("DistributionC", e, m, X, "x2" == O || "p2" == O, a),
      (B.innerHTML = L + "<br>" + M + "<br><br>" + like()),
      GEBI("messageC").scrollIntoView(),
      $(document).ready(function () {
        // $('[data-toggle="tooltip"]').tooltip();
      });
  } else alert("error"), set_variable("messageC", V);
  // var j = performance.now(),
  // J = Math.round(j - w);
  // gtag("event", "Distribution", {
  //   event_category: e,
  //   event_label: O,
  //   value: J,
  // });
}
function drawDistributionCalc(e, r, a, o, i, l) {
  (v = l), (a *= 1), (o *= 1);
  var t,
    n,
    s,
    d,
    b,
    u,
    p,
    c = 0.01;
  deltaColor = 1 * c;
  var m,
    _ = findO(ODistribution, "type", r),
    x = ODistribution[_];
  if ("Chi-Square" == r || "F" == r || x.discrete || "weibull" == r) {
    s = "Chi-Square" == r ? 0.1 : 1;
    var g = { min: 0, max: 0 };
    if (
      (find_minmax(0.01, r, l, g, s),
      (t = g.min),
      debug.log("min" + t),
      (n = g.max),
      debug.log("max" + n),
      x.discrete)
    ) {
      var f = v.n;
      for (
        f = "binomial" == r ? v.n : 2 * v.lambda;
        n <= f && 0.001 < GenFunction(r, n, l);

      )
        n++;
      for (; 0 < t && 0.001 < GenFunction(r, t, l); ) t--;
      c = 1;
    }
  } else
    (n =
      "Normal" == r
        ? ((t = x.min * v.std + v.mean),
          debug.log("min" + t),
          x.max * v.std + v.mean)
        : ((t = x.min), debug.log("min" + t), x.max)),
      debug.log("max" + n);
  if (
    ((u = i
      ? [["value", "P(X≤x₁)", "P(x₁<X<x₂)", "P(X>x₂)", "x₁,x₂"]]
      : [["value", "P(X≤x₁)", "P(X>x₁)", "x₁"]]),
    i)
  )
    for (p = ["green", "red", "blue", "black"], d = t; d < n; d += c)
      (b = pdf(r, d, l)),
        d <= a + deltaColor && d >= a - deltaColor
          ? u.push([1 * d, 0, 0, 0, 1 * b])
          : d <= o + deltaColor && d >= o - deltaColor
          ? u.push([1 * d, 0, 0, 0, 1 * b])
          : d <= a
          ? u.push([1 * d, 1 * b, 0, 0, 0])
          : a < d && d <= o
          ? u.push([1 * d, 0, 1 * b, 0, 0])
          : u.push([1 * d, 0, 0, 1 * b, 0]);
  else
    for (p = ["red", "green", "black"], d = t; d < n; d += c)
      (b = GenFunction(r, d, l)),
        d <= a + deltaColor && d >= a - deltaColor
          ? u.push([1 * d, 0, 0, 1 * b])
          : d <= a
          ? u.push([1 * d, 1 * b, 0, 0])
          : u.push([1 * d, 0, 1 * b, 0]);
  (m = "Chi-Square" == r ? "χ²" : r),
    "Chi-Square" == r
      ? (m += "(" + v.df + ")")
      : "F" == r && (m += "(" + v.df1 + "," + v.df2 + ")");
  var h = cap_first_letter(m) + " distribution";
  x.discrete
    ? ((type1 = "ColumnChart"), (isStacked1 = !0))
    : ((type1 = "AreaChart"), (isStacked1 = !1));
  var k = {
    title: h,
    bar: { groupWidth: "95%" },
    isStacked: isStacked1,
    curveType: "function",
    legend: { position: "bottom" },
    colors: p,
  };
  draw_chart2(type1, u, e, k, !1);
}
