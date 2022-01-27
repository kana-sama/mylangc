### Build

```bash
(cd runtime; make)
hpack
cabal install
```

### Commands

```
  mylangc asm SOURCE.mylang
    Generate GAS assembly file
  mylangc build SOURCE.mylang
    Build executable
  mylangc run SOURCE.mylang
    Evaluate compilled executable
```

### Examples

```
x := 2 * 10 / 2;
read(y);
x := x + y;
write(x);
write(42)
```
