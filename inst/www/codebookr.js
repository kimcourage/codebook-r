/* Navigation and focus only; never write data, approvals or operation settings. */
$(function () {
  $(document).on('click', '[data-ui-tab]', function (event) {
    event.preventDefault();
    var value = $(this).attr('data-ui-tab');
    $('#main_tabs a[data-value]').filter(function () {
      return $(this).attr('data-value') === value;
    }).tab('show');
  });
  $(document).on('shown.bs.tab', '#main_tabs a[data-toggle="tab"]', function () {
    var panel = document.getElementById($(this).attr('href').slice(1));
    var heading = panel && panel.querySelector('h2');
    if (heading) heading.focus({ preventScroll: true });
    window.scrollTo(0, 0);
  });
  $(document).on('keydown', '#main_tabs a[data-toggle="tab"]', function (event) {
    if (!['ArrowLeft', 'ArrowRight', 'Home', 'End'].includes(event.key)) return;
    var tabs = $('#main_tabs a[data-toggle="tab"]');
    var index = tabs.index(this);
    if (event.key === 'Home') index = 0;
    else if (event.key === 'End') index = tabs.length - 1;
    else index = (index + (event.key === 'ArrowRight' ? 1 : -1) + tabs.length) % tabs.length;
    event.preventDefault();
    tabs.eq(index).tab('show').trigger('focus');
  });
});
