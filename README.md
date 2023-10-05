# What is This?

A minimalist lisp language and computing environment that runs in the browser.

Live: https://midnightsystem.com

Docs: https://domain-j.com/Midnight-System/uuid/9885fa97-705e-4d68-a0cb-329c3e4b233e

# Development model

No PRs without asking first please, I want to make sure I understand everything in the codebase.

# Setup

```
npm install
npm run postinstall
```

# Run

In separate terminals:

```
npm run esbuild-dev-watch
```

```
npm run tailwind-watch
```

```
npm run generate-purs-watch
```

Port 8000:
```
npm run serve
```

Port 3000:
```
./proxy
```

Go to http://127.0.0.1:3000/

# Test

```
npm run test-watch
```
