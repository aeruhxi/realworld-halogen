function main() {
 require("./output/Main").main();
}


if (module.hot) {
  module.hot.dispose(() => {
    window.location.reload();
  });
}
main()