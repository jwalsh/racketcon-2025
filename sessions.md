# RacketCon 2025 Sessions

## Saturday, 9:00am EDT - Keynote

**How Cloudflare Uses Racket and Rosette to Verify DNS Changes**

**Speakers:** James Larisch and Suleman Ahmad

### Abstract

Since 2022, Cloudflare has used Racket and Rosette to prevent DNS-related bugs. Cloudflare engineers express desired DNS behavior as small programs called policies, written in a custom DSL called topaz-lang. Topaz-lang policies are executed in real-time on Cloudflare's global edge network in response to live DNS queries. But before deployment, all policies are checked for bugs using a verifier we wrote in Rosette, a solver-aided Racket #lang.

In this talk, we describe our experience writing and using Racket in production at Cloudflare. We describe why managing DNS behavior at Cloudflare scale is so challenging, and how these challenges motivated topaz-lang and its parent system Topaz. We discuss why we chose Racket (and Rosette) and the types of bugs our Rosette verifier detects. Finally, we reflect on why making changes to our verifier remains daunting for many software engineers.

### Speaker Bios

**Suleman Ahmad** is a Research Engineer at Cloudflare, working at the intersection of systems engineering and Internet security. He holds a Master's degree from the University of Wisconsin–Madison, where he focused on analyzing security and privacy challenges in large-scale Internet architectures and engineering scalable measurement platforms. It was during his master's studies that he developed an appreciation for functional programming and its practical application to verifiable distributed systems.

**James Larisch** is a systems/security researcher and programming language fanboy. He developed his appreciation for the functional style (and Racket) during his undergraduate degree at Northeastern University. He received his PhD in Computer Science from Harvard University, where one of his projects involved bringing Prolog to the Web Public Key Infrastructure. He is currently a Research Engineer at Cloudflare, where he works on the Web PKI, distributed systems, and a bit of formal methods.

### Links

- [Rosette Documentation](https://docs.racket-lang.org/rosette-guide/)
- [Cloudflare Blog](https://blog.cloudflare.com/)
- [Cloudflare Research](https://research.cloudflare.com/)

---

## Saturday, 10:15am EDT

**Compositional Object Oriented Prototypes**

**Speaker:** François-René Rideau

### Abstract

We reconstruct a theory of object-orientation from first principles, as modularity and extensibility together. Mixin inheritance then appears as a natural embodiment of these joined principles expressed in the lambda-calculus. Further OO concepts such as prototypes, classes, single or multiple inheritance, multiple dispatch, method combinations and more naturally follow. Interestingly, many misconceptions about OO can also be dispelled, and we find that the simplest and most natural context for OO is pure lazy dynamic functional programming, without classes, and even without objects(!). A Scheme and/or Racket prototype (ha!) of these ideas will be presented.

### Speaker Bio

**François-René Rideau (Faré)** - Not fitting in French Academia due to his penchant for dynamic languages, Faré learned how (not) to build software in Corporate America (ITA, Google, Bridgewater), and eventually became his own startup entrepreneur in the domain of secure blockchain architecture. Trained in Programming Language Semantics and Distributed Systems, Faré completed but never defended a thesis on Reflective Systems. Once author of versions 2 and 3 of the build system ASDF at the heart of all Common Lisp free software, he is now co-maintainer of Gerbil Scheme. Unsettled by online debates between OO vs FP back when he was a student at ENS.fr, he finally discovered twenty years later the essence of OO thanks to Jsonnet and Nix, and, trying to share his insight and digging into old bibliography, became despite himself an expert on Object-Orientation.

### Links

- [Gerbil Scheme](https://cons.io/)
- [ASDF Build System](https://common-lisp.net/project/asdf/)
- [Faré's Blog](https://fare.livejournal.com/)

---

## Saturday, 10:45am EDT
