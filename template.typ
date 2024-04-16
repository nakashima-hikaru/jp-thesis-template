// Store theorem environment numbering
#let thmcounters = state("thm", ("counters": ("heading": ()), "latest": ()))

// Setting theorem environment
#let thmenv(identifier, base, base_level, fmt) = {
  let global_numbering = numbering

  return (
    ..args,
    body,
    number: auto,
    numbering: "1.1",
    refnumbering: auto,
    supplement: identifier,
    base: base,
    base_level: base_level,
  ) => {
    let name = none
    if args != none and args.pos().len() > 0 {
      name = args.pos().first()
    }
    if refnumbering == auto {
      refnumbering = numbering
    }
    let result = none
    if number == auto and numbering == none {
      number = none
    }
    if number == auto and numbering != none {
      result = locate(loc => {
        return thmcounters.update(thmpair => {
          let counters = thmpair.at("counters")
          // Manually update heading counter
          counters.at("heading") = counter(heading).at(loc)
          if not identifier in counters.keys() {
            counters.insert(identifier, (0,))
          }

          let tc = counters.at(identifier)
          if base != none {
            let bc = counters.at(base)

            // Pad or chop the base count
            if base_level != none {
              if bc.len() < base_level {
                bc = bc + (0,) * (base_level - bc.len())
              } else if bc.len() > base_level {
                bc = bc.slice(0, base_level)
              }
            }

            // Reset counter if the base counter has updated
            if tc.slice(0, -1) == bc {
              counters.at(identifier) = (..bc, tc.last() + 1)
            } else {
              counters.at(identifier) = (..bc, 1)
            }
          } else {
            // If we have no base counter, just count one level
            counters.at(identifier) = (tc.last() + 1,)
            let latest = counters.at(identifier)
          }

          let latest = counters.at(identifier)
          return ("counters": counters, "latest": latest)
        })
      })

      number = thmcounters.display(x => {
        return global_numbering(numbering, ..x.at("latest"))
      })
    }

    return figure(
      result + // hacky!
        fmt(name, number, body, ..args.named()) + [#metadata(identifier) <meta:thmenvcounter>],
      kind: "thmenv",
      outlined: false,
      caption: none,
      supplement: supplement,
      numbering: refnumbering,
    )
  }
}

// Definition of theorem box
#let thmbox(
  identifier,
  head,
  ..blockargs,
  supplement: auto,
  padding: (top: 0.5em, bottom: 0.5em),
  namefmt: x => [(#x)],
  titlefmt: strong,
  bodyfmt: x => x,
  separator: [#h(0.1em):#h(0.2em)],
  base: "heading",
  base_level: none,
) = {
  if supplement == auto {
    supplement = head
  }
  let boxfmt(name, number, body, title: auto) = {
    if not name == none {
      name = [ #namefmt(name) ]
    } else {
      name = []
    }
    if title == auto {
      title = head
    }
    if not number == none {
      title += " " + number
    }
    title = titlefmt(title)
    body = bodyfmt(body)
    pad(..padding, block(
      width: 100%,
      inset: 1.2em,
      radius: 0.3em,
      breakable: false,
      ..blockargs.named(),
      [#align(left)[#title#separator#name]#align(left)[#body]],
    ))
  }
  return thmenv(identifier, base, base_level, boxfmt).with(supplement: supplement)
}

// Setting plain version
#let thmplain = thmbox.with(
  padding: (top: 0em, bottom: 0em),
  breakable: true,
  inset: (top: 0em, left: 1.2em, right: 1.2em),
  namefmt: name => emph([(#name)]),
  titlefmt: emph,
)

// Counting equation number
#let equation_num(_) = {
  locate(loc => {
    let chapt = counter(heading).at(loc).at(0)
    let c = counter(math.equation)
    let n = c.at(loc).at(0)
    "(" + str(chapt) + "." + str(n) + ")"
  })
}

// Counting table number
#let table_num(_) = {
  locate(loc => {
    let chapt = counter(heading).at(loc).at(0)
    let c = counter("table-chapter" + str(chapt))
    let n = c.at(loc).at(0)
    str(chapt) + "." + str(n + 1)
  })
}

// Counting image number
#let image_num(_) = {
  locate(loc => {
    let chapt = counter(heading).at(loc).at(0)
    let c = counter("image-chapter" + str(chapt))
    let n = c.at(loc).at(0)
    str(chapt) + "." + str(n + 1)
  })
}

// Definition of table format
#let tbl(tbl, caption: "") = {
  figure(
    tbl,
    caption: caption,
    supplement: [表],
    numbering: table_num,
    kind: "table",
  )
}

// Definition of image format
#let img(img, caption: "") = {
  figure(
    img,
    caption: caption,
    supplement: [図],
    numbering: image_num,
    kind: "image",
  )
}

// Definition of abstruct page
#let abstract_page(abstract, keywords: ()) = {
  if abstract != [] {
    show <_abstract_>: {
      align(center)[
        #text(size: 20pt, "概要")
      ]
    }
    [= 概要 <_abstract_>]

    v(30pt)
    set text(size: 12pt)
    h(1em)
    abstract
    par(first-line-indent: 0em)[
      #text(weight: "bold", size: 12pt)[
        キーワード:
        #keywords.join(", ")
      ]
    ]
  }
}

// Setting empty par
#let empty_par() = {
  v(-1em)
  box()
}

// Construction of paper
#let thesis_template(
  // The master thesis title.
  title: str,
  // The paper`s author.
  author: str,
  // The author's information
  affiliation: str,
  date: datetime,
  // Abstruct
  abstract: array,
  keywords: (),
  // The paper size to use.
  paper-size: "a4",
  // The path to a bibliography file if you want to cite some external
  // works.
  bibliography-file: none,
  // The paper's content.
  body,
) = {
  // citation number
  show ref: it => {
    if it.element != none and it.element.func() == figure {
      let el = it.element
      let loc = el.location()
      let chapt = counter(heading).at(loc).at(0)

      link(
        loc,
      )[#if el.kind == "image" or el.kind == "table" {
          // counting
          let num = counter(el.kind + "-chapter" + str(chapt)).at(loc).at(0) + 1
          it.element.supplement
          " "
          str(chapt)
          "."
          str(num)
        } else if el.kind == "thmenv" {
          let thms = query(selector(<meta:thmenvcounter>).after(loc), loc)
          let number = thmcounters.at(thms.first().location()).at("latest")
          it.element.supplement
          " "
          numbering(it.element.numbering, ..number)
        } else {
          it
        }
      ]
    } else if it.element != none and it.element.func() == math.equation {
      let el = it.element
      let loc = el.location()
      let chapt = counter(heading).at(loc).at(0)
      let num = counter(math.equation).at(loc).at(0)

      it.element.supplement
      " ("
      str(chapt)
      "."
      str(num)
      ")"
    } else if it.element != none and it.element.func() == heading {
      let el = it.element
      let loc = el.location()
      let num = numbering(el.numbering, ..counter(heading).at(loc))
      if el.level == 1 {
        str(num)
        "章"
      } else if el.level == 2 {
        str(num)
        "節"
      } else if el.level == 3 {
        str(num)
        "項"
      }
    } else {
      it
    }
  }

  // counting caption number
  show figure: it => {
    set align(center)
    if it.kind == "image" {
      set text(size: 12pt)
      it.body
      it.supplement
      " " + it.counter.display(it.numbering)
      " " + it.caption.body
      locate(loc => {
        let chapt = counter(heading).at(loc).at(0)
        let c = counter("image-chapter" + str(chapt))
        c.step()
      })
    } else if it.kind == "table" {
      set text(size: 12pt)
      it.supplement
      " " + it.counter.display(it.numbering)
      " " + it.caption.body
      set text(size: 10.5pt)
      it.body
      locate(loc => {
        let chapt = counter(heading).at(loc).at(0)
        let c = counter("table-chapter" + str(chapt))
        c.step()
      })
    } else {
      it
    }
  }

  // Set the document's metadata.
  set document(title: title, author: author)

  // Set the body font. TeX Gyre Pagella is a free alternative
  // to Palatino.
  set text(font: (
    // "Nimbus Roman",
    "Linux Libertine",
    "Hiragino Mincho ProN",
    // "MS Mincho",
    // "Noto Serif CJK JP",
  ), size: 12pt, lang: "ja")
  // set text(font: "Linux Libertine", lang: "en")
  // Configure the page properties.
  set page(paper: paper-size, margin: (bottom: 1.75cm, top: 2.25cm))

  // The first page.
  align(
    center,
  )[
    #v(80pt)
    #text(size: 16pt)[
      #affiliation
    ]

    #v(40pt)
    #text(size: 22pt)[
      #title
    ]
    #v(50pt)
    #text(size: 16pt)[
      #author
    ]

    #v(40pt)
    #text(
      size: 16pt,
    )[
      #date.display("[year padding:none]年 [month padding:none]月 [day padding:none]日")
    ]
    #pagebreak()
  ]

  set page(footer: [
    #align(center)[#counter(page).display("i")]
  ])

  counter(page).update(1)
  // Show abstruct
  abstract_page(abstract, keywords: keywords)

  // Configure paragraph properties.
  set par(leading: 0.78em, first-line-indent: 12pt, justify: true)
  show par: set block(spacing: 0.78em)

  // Configure chapter headings.
  set heading(numbering: (..nums) => {
    let ret = nums.pos().map(str).join(".")
    if ret.len() > 1 {
      ret += " "
    }
    return ret
  })
  show heading.where(level: 1): it => {
    pagebreak()
    counter(math.equation).update(0)
    set text(weight: "bold", size: 20pt)
    set block(spacing: 1.5em)
    let pre_chapt = if it.numbering != none {
      text()[
        #v(50pt)
        第
        #numbering(it.numbering, ..counter(heading).at(it.location()))
        章
      ]
    } else { none }
    text()[
      #pre_chapt \
      #it.body \
      #v(50pt)
    ]
  }
  show heading.where(level: 2): it => {
    set text(weight: "bold", size: 16pt)
    set block(above: 1.5em, below: 1.5em)
    it
  }

  show heading: it => {
    set text(weight: "bold", size: 14pt)
    set block(above: 1.5em, below: 1.5em)
    it
  } + empty_par()

  // Start with a chapter outline.
  outline(title: "目次")

  set page(footer: [
    #align(center)[#counter(page).display("1")]
  ])

  let ht-first = state("page-first-section", [])
  let ht-last = state("page-last-section", [])

  set page(
    header: locate(
      loc => [
        // find first heading of level 1 on current page
        #let firstheading = query(heading.where(level: 1), loc).find(h => h.location().page() == loc.page())
        // find last heading of level 1 on current page
        #let last-heading = query(heading.where(level: 1), loc).rev().find(h => h.location().page() == loc.page())
        // test if the find function returned none (i.e. no headings on this page)
        #{
          if not firstheading == none {
            ht-first.update(
              [
                // change style here if update needed section per section
                #align(
                  right,
                )[第 #counter(heading).at(firstheading.location()).at(0) 章 - #firstheading.body]
              ],
            )
            ht-last.update(
              [
                #align(
                  right,
                )[
                  // change style here if update needed section per section
                  第 #counter(heading).at(last-heading.location()).at(0) 章 - #last-heading.body
                ]
              ],
            )
            // if one or more headings on the page, use first heading
            // change style here if update needed page per page
            ht-first.display()
          } else {
            // no headings on the page, use last heading from variable
            // change style here if update needed page per page
            ht-last.display()
          }
        }
      ],
    ),
  )

  counter(page).update(1)

  set math.equation(supplement: [式], numbering: equation_num)

  body

  // Display bibliography.
  if bibliography-file != none {
    show bibliography: set text(12pt)
    show bibliography: set page(header: none)
    bibliography(
      bibliography-file,
      title: "参考文献",
      style: "american-physics-society",
      full: true,
    )
  }
}

