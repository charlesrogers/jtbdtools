# Append a JTBD footer to a ggplot caption

Internal helper. Combines the plot's existing caption with a
[`jtbd_footer()`](https://charlesrogers.github.io/jtbdtools/reference/jtbd_footer.md)
string, joining them on a newline so both stay readable.

## Usage

``` r
.merge_caption(existing, n = NULL, study = NULL, extra = NULL)
```

## Arguments

- existing:

  The current caption (may be NULL or empty).

- n, study, extra:

  Forwarded to
  [`jtbd_footer()`](https://charlesrogers.github.io/jtbdtools/reference/jtbd_footer.md).

## Value

A character string (or NULL if everything is empty).
