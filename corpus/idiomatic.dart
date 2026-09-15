// idiomatic.dart — M4 idiomatic-mode corpus: widget classes, state, threading.
import 'package:flutter/material.dart';

class Greeting extends StatelessWidget {
  final String name;
  const Greeting({super.key, required this.name});
  @override
  Widget build(BuildContext context) {
    return Padding(
      padding: const EdgeInsets.all(8),
      child: Center(
          child: DecoratedBox(
              decoration: const BoxDecoration(), child: Text('Hello $name'))),
    );
  }
}

class Counter extends StatefulWidget {
  final String title;
  const Counter({super.key, required this.title});
  @override
  State<Counter> createState() => CounterState();
}

class CounterState extends State<Counter> {
  int count = 0;
  int step = 1;
  void increment() {
    setState(() {
      count += step;
    });
  }
  int doubled() => count * 2;
  Future<void> ping() async {
    await Future.delayed(const Duration(seconds: 1));
    setState(() {
      count++;
    });
  }
  @override
  Widget build(BuildContext context) {
    return Scaffold(
      appBar: AppBar(title: Text(widget.title)),
      body: Center(child: Text('Count: $count (${doubled()})')),
      floatingActionButton: FloatingActionButton(
        onPressed: () => setState(() => count += 2),
        child: const Icon(Icons.add),
      ),
    );
  }
}

class Banner extends StatefulWidget {
  const Banner();
  @override
  State<Banner> createState() => BannerState();
}

class BannerState extends State<Banner> {
  @override
  Widget build(BuildContext context) => const Center(child: Text('banner'));
}

class Ticker extends StatefulWidget {
  const Ticker({super.key});
  @override
  State<Ticker> createState() => TickerState();
}

class TickerState extends State<Ticker> {
  final StringBuffer buf = StringBuffer();
  void bump() {
    setState(() {
      buf.write('x');
    });
  }
  @override
  Widget build(BuildContext context) {
    return TextButton(
      onPressed: bump,
      child: Text('ticks: ${buf.length}'),
    );
  }
}

class Page extends StatelessWidget {
  const Page();
  @override
  Widget build(BuildContext context) {
    return MaterialApp(
      title: 'Demo',
      theme: ThemeData(primarySwatch: Colors.blue),
      home: Counter(key: null, title: 'Counter'),
    );
  }
}

class Legacy extends StatelessWidget {
  const Legacy();
  int get answer => 42;
  @override
  Widget build(BuildContext context) => const Text('legacy');
}

class Plain {
  int value = 0;
  Plain(this.value);
  int double() => value * 2;
}

int modulo(int a, int b) => a % b;
