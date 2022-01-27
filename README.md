### Build

```bash
(cd runtime; make)
hpack
cabal install
```

### Commands

```
  mylangc build SOURCE.mylang
  mylangc run SOURCE.mylang
  mylangc asm SOURCE.mylang
    Generate GAS file
```

### Examples

```
x := 2 * 10 / 2;
read(y);
x := x + y;
write(x);
write(42)
```
