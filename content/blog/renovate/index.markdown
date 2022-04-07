---
title: A Fresh Lick of Paint
author: Carl Goodwin
date: '2022-04-10'
slug: renovate
categories:
  - R
tags:
  - web site
summary: A couple of years ago I [moved house](/blog/plunge) from Wordpress to [Blogdown](https://bookdown.org/yihui/blogdown/). It's a less stressful life and I plan to stay. [Hugo Academic](https://academic-demo.netlify.app) served me well, but sometimes you just need a fresh coat of paint.
lastmod: '2022-04-07'
draft: false
featured: false
---

![](/blog/renovate/featured.GIF)

A couple of years ago I [moved house](/blog/plunge) from Wordpress to [Blogdown](https://bookdown.org/yihui/blogdown/). It's proved to be a much less stressful life and I plan to stay. [Hugo Academic](https://academic-demo.netlify.app) served me well, but sometimes you just need a fresh coat of paint. I liked the look of [Hugo Apéro](https://hugo-apero-docs.netlify.app).

Apéro feels simpler and has an elegant design with well-chosen themes and fonts.

I like to add my own digital art to both the site and Rmarkdown projects and Apéro gives me more flexibility here.  It naturally accommodates GIF animations, for example, on my [home page](/.) and in my project and [blog](/blog/) lists. 

Apéro does not yet have a naturally built-in dark mode option, nor a tag cloud; features I used in Academic. But these are not super important for me.

The upgrade approach I took was to create a brand new blogdown project in RStudio with the Apéro theme and then copy over and re-knit my projects one by one. This worked well because every project needed at least one change as a direct consequence of the move and re-opening each project also prompted other beneficial updates. I left my original Hugo Academic RStudio project untouched so I could easily revert to it.

Running `blogdown::check_site()` is highly worthwhile and runs a number of checks to ensure you're good-to-go.

I focused first on manual deployment, i.e. dragging the Public folder to Netlify, rather than going straight to continuous deployment via Github. Doing it this way would narrow the potential cause of any problems when doing the latter. I also deployed to one of Netlify's auto-generated site names, so my live Academic blog remained unaffected.

In Hugo Academic, each project's (or post's) feature image rendered automatically in both the project list page and in the individual project. In Apéro, I needed to add `![](pathname)` to the Rmarkdown file to render the image in an individual project or post. I actually prefer this approach because it means the image then also appears when re-publishing to a blog aggregator.

Initially a few things did not render correctly, e.g. syntax highlighting, which it turned out required renaming the `index.Rmd` files to `index.Rmarkdown`. And when the manual deployment to Netlify got stuck uploading, I realised I also needed to change the `.Rprofile` to `blogdown.method = 'markdown'` rather than `blogdown.method = 'html'`.

Once the manual deployment to Netlify was working, I then moved on to continuous deployment via Github. Part of the motivation for this was so I could switch the commenting engine from Disqus to [utterance.es](https://utteranc.es). The latter is a more elegant fit with the Apéro design and has some nice advantages. 

And because I wanted to deploy a *pre-existing* RStudio project to Github, rather than following the usual Github-first practice, I found this [guidance](https://happygitwithr.com/existing-github-first.html) helpful.

The Netlify deployment via Github initially also failed with a "Base directory does not exist" message. The fix there was to leave the Base directory in Netlify's Build settings blank rather than using the repo URL (which it already had under Current repository).

![](/blog/renovate/netlify.png)

One more thing I needed to do was set up this `static/_redirects` file. In this way, bookmarks for, say, `category/r` or `tag/statistical-inference` (following the taxonomy for Academic) would go to `categories/r` or `tags/statistical-inference`.


```
## /category/*  /categories/:splat
## /tag/*       /tags/:splat
```

Then finally I could pack away my paint pots, paint roller and step ladder, put my feet up and enjoy my newly-renovated blogdown home.

