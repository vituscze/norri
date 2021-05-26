In order to show that the translation preserves the semantics of the source language, we first
need to show that it preserves substitution. In particular, we would like to show that the
substitution can be translated into the template instantiation. However, such fact cannot be
established directly since the resulting templates can only be instantiated with type names.

Instead of showing that the corresponding free occurences of the substituted variable
are replaced with the translation of the substituted term, we will show that these
variables refer to the correct translated subterm, although indirectly. In the following,
we will use `[[x := _N]]` to represent the template parameter substitution.

That is, we expect

    translate(M[x := N]) = translate(M)[[x := _N]]

where `_N` is a type name corresponding to the translation of `N`.

If the term is a variable whose name matches that of the substituted variable, we get:

        translate(x[x := N])
    ==
        translate(N)
    ==
        struct _N { translate(N) }
        using type = _N::type;
    ==
        translate(x)[[x := _N]]

If the name does not match, we get:

        translate(y[x := N])
    ==
        translate(y)
    ==
        using type = typename y::type;
    ==
        translate(y)[[x := _N]]

If the term is a let expression, we get:

        translate((let y = M1 in M2)[x := N])
    ==
        translate(let y = M1[x := N] in M2[x := N])
    ==
        struct y { translate(M1[x := N]) };
        translate(M2[x := N])
    ==
        struct y { translate(M1)[[x := _N]] };
        translate(M2)[[x := _N]]
    ==
        translate(let y = M1 in M2)[[x := _N]]

Other cases are similar. Note that we do not have to deal with variable capture
or shadowing since the variables are required to be fresh.

Now we can show that beta equivalence of terms of the source language translates
into equivalence of the translated metaprograms. Reflexivity, transitivity and
symmetry follow trivially. We also need to establish the congruence property for
all term formers, such as `M = N => \x. M = \x. N`.

        translate(\x. M)
    ==
        struct type {
          template <typename x>
          struct apply {
            translate(M)
          };
        };
    ==
        struct type {
          template <typename x>
          struct apply {
            translate(N)
          };
        };
    ==
        translate(\x. N)

Congruence for other term formers can be established similarly. We are now ready
to consider the beta rules themselves.

### Function Application

        translate((\x. M) N)
    ==
        struct _FN {
          struct type {
            template <typename x>
            struct apply {
              translate(M)
            };
          };
        };

        struct _N { translate(N) };

        using type = typename _FN::type::template apply<_N>::type;
    ==

At this point, we need to instantiate the inner template `apply` with `_N`:

        struct _N { translate(N) };

        struct _apply_inst {
          translate(M)[[x := _N]]
        };

        using type = typename _apply_inst::type;
    ==

`_apply_inst::type` now simply refers to the translation of `M`:

        translate(M)[[x := _N]]
    ==
        translate(M[x := N])
      
### Let Expression
      
        translate(let x = M in N)
    ==
        struct x { translate(M) };
        translate(N)
    ==
        struct _M { translate(M) }
        translate(N)[[x := _M]]
    ==
        translate(N[x := M])

### Case Analysis

Without loss of generality, we assume that the input of the case analysis is a value whose constructor only has one field.

        translate(case (D(N)) { D x -> M })
    ==
        template <typename>
        struct _case;

        template <typename f>
        struct _case<D<f>> {
          struct x { using type = f; };
          translate(M)
        };

        // Other cases that are less specific and thus won't be selected.
        template <...>
        struct _case<...> { ... };

        struct _DN {
          struct _N { translate(N) };
          using type = D<typename _N::type>;  
        };

        using type = typename _case<typename _DN::type>::type;
    ==

At this point, we can simplify the type `typename _DN::type`:

        template <typename f>
        struct _case<D<f>> {
          struct x { using type = f; };
          translate(M)
        };

        struct _N { translate(N) };

        using type = typename _case<D<typename _N::type>>::type;
    ==

Now we need to instantiate the partial specialization of the template `_case` with `D<typename _N::type>`:

        struct _N { translate(N) };

        struct _case_inst {
          struct x { using type = typename _N::type; };
          translate(M)
        };

        using type = typename _case_inst::type;
    ==
        struct _case_inst {
          struct x { translate(N) };
          translate(M)
        };

        using type = typename _case_inst::type;
    ==
        translate(M)[[x := N]]
    ==
        translate(M[x := N])
