# Build Your Own Calculator

<!-- <details> -->
<!-- <summary>Challenge ??</summary> -->

### [Coding Challenge - Build Your Own Calculator](https://codingchallenges.fyi/challenges/challenge-calculator/)

This challenge is to build your own calculator. It could be a command line tool, desktop application or web based.

To complete this challenge you’ll need to parse a mathematical expression and then perform the relevant calculations before returning the answer to the user.

For example, the user will be able to input: 2 * 3 + 4 and get back 10, or input 10 / (6 - 1) and get back 2.

Completing this challenge will give you the chance to make use of the stack data structure in a real-world application.

#### Step Zero

In this introductory step you’re going to set your environment up ready to begin developing and testing your solution.

I’ll leave you to choose your target platform, setup your IDE / editor of choice and programming language of choice.

#### Step 1

In this step your goal is to handle four simple expressions:

<pre>
<code>1 + 2
2 - 1
2 * 3
3 / 2 </code>
</pre>

You’ll need to handle these as input and return the correct answer. Don’t forget to consider the difference between integers and floating points as well as the potentially invalid input.

When you’re running your tests, don’t forget to quote the expression so the shell doesn’t expand the <code>*</code> character.

<pre>
<code>% calc '2 * 3'
6</code>
</pre>

Now, just before you start coding I want to give you a choice. You could proceed with this challenge writing the code for Step 1 first, perhaps doing test-driven development (TDD), or you can read on to take a look at all the steps, which may change your approach.

It’s your ~~adventure~~ coding challenge, you choose!

#### Step 2

In this step your goal is to handle some more complex expressions:

<pre>
<code>1 + 1 * 5
(1 + 1) * 5</code>
</pre>

At this point, a basic implementation done via TDD might need quite a bit of refactoring. That’s cool, refactoring is a
great skill to learn.

A great way to handle these test cases is to convert the input from infix notation to postfix notation (also known as
reverse polish notation). Doing so makes it much easier to handle precedence and parentheses. For example
<code>(1 * 2) - (3 * 4)</code> becomes <code>1 2 * 3 4 * -</code>.

Once you have the input in reverse polish notation in’s much easier to perform the calculations by simply pushing onto a
stack until you reach an operator then popping off the values, i.e.:

<ol>
<li>Input tokens: <code>2 *</code>1 Stack: Empty - take the first token from the input, as it’s not an operator push it onto the stack.</li>
<li>Input tokens: <code>2 *</code> Stack: <code>1</code> - now take the second token, as it’s not an operator push it onto the stack.</li>
<li>Input tokens: <code>*</code> Stack: <code>1 2</code> - the next token is an operator, pop two items off the stack and apply the operator, push the result onto the stack.</li>
<li>Input tokens: Empty Stack: <code>2</code></li>
</ol>

Before you can do that, you’ll need to parse the input tokens and convert the ordering to postfix. This can be done with
the shunting yard algorithm which is also implemented using a stack. For example:

<ol>
    <li>Input: <code>1 * 2</code></li>
    <li>Push <code>1</code> to the output queue (whenever a number is read it is pushed to the output).</li>
    <li>Push <code>*</code> on to the operator stack.</li>
    <li>Push <code>2</code> to the output.</li>
    <li>After reading the expression, pop the operators off the stack and add them to the output.</li >
    <li>In this case there is only one, <code>*</code>.</li>
    <li>Output: <code>1 2 *</code></li>
</ol>

This demonstrates the essence of how the algorithm works:

<ul>
<li>Numbers are pushed to the output when they are read.</li>
<li>When we finish reading the expression, pop all operators off the stack and append them to the output.</li>
</ul>

The full algorithm is explained on Wikipedia’s Shunting Yard Algorithm page, along with some very detailed examples.

#### Step 3

In this step your goal is to handle some more complex expressions. I encourage you to devise your own test cases and to
implement them as automated tests.

You can try adding brackets for precedence as well as more complex operators like sin, cos and tan.

#### Share Your Solutions! [#](https://codingchallenges.fyi/challenges/challenge-calculator/#share-your-solutions)

If you think your solution is an example of the developers can learn from please share it, put it on GitHub, GitLab or
elsewhere. Then let me know - ping me a message via Twitter or LinkedIn or just post about it there and tag me.
<!-- </details> -->

<hr/>

My discussion and notes:

- [Shunting Yard Algorithm]( https://en.wikipedia.org/wiki/Shunting_yard_algorithm )
  - 2 stacks
    - output / operand stack - collects numbers and variables names
    - operator stack = collects operation and parentheses
    - parentheses closer as algorithm progresses
    - operator precedence unwinding as algorithm progress

<hr/>

## running the app

- docker run
- `docker build -t my-app .`
- `podman machine init`
- `podman machine start`
- `podman build -t calculator .`
- `podman run --name calculator -i calculator:latest`
