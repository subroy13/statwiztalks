// Shared controls align playback positions when starting the comparison.
// Native video controls remain available for inspecting individual results.
document.addEventListener('DOMContentLoaded', () => {
  const videos = [...document.querySelectorAll('.video-comparison video')];
  const status = document.querySelector('.video-status');
  const pauseAll = () => videos.forEach(video => video.pause());
  document.querySelectorAll('[data-video-action]').forEach(button => {
    button.addEventListener('click', async () => {
      const action = button.dataset.videoAction;
      if (action === 'pause') { pauseAll(); return; }
      const time = action === 'restart' ? 0 : videos[0].currentTime;
      pauseAll();
      videos.forEach(video => { video.currentTime = time; });
      const results = await Promise.allSettled(videos.map(video => video.play()));
      if (results.some(result => result.status === 'rejected')) {
        pauseAll();
        status.textContent = 'Playback failed. Try the individual video controls.';
      } else { status.textContent = ''; }
    });
  });
  // Pause on leaving the comparison slide; no video starts automatically.
  if (typeof Reveal !== 'undefined') {
    Reveal.on('slidechanged', event => {
      if (!event.currentSlide.querySelector('.video-comparison')) pauseAll();
    });
  }
});
