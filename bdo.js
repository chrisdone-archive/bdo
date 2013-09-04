/// bdo.el
/// Do things to a browser page from Emacs.

// Copyright (c) 2012 Chris Done. All rights reserved.

// THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
// "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
// LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
// FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL
// Chris Done BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
// SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
// LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES/ LOSS OF
// USE, DATA, OR PROFITS/ OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
// ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
// OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT
// OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
// SUCH DAMAGE.

var bdo = {};

/*******************************************************************************
 * Initialize everything.
 */
bdo.init = function(){
  $(document).ready(function(){
    bdo.sendLinks();
    bdo.poll();
  });
};

/*******************************************************************************
 * Send the hrefs of all the CSS link elements in the page.
 */
bdo.sendLinks = function(){
  var hrefs = [];
  $('link[type="text/css"]').each(function(){
    if($(this).attr('rel') == 'stylesheet') {
      hrefs.push($(this).attr('href'));
    }
  });
  $.post(bdo.host + 'links',{links:hrefs.join('\n')},function(response){
    bdo.log("Posted links: " + JSON.stringify(hrefs));
    bdo.log("Links reply: %s",response);
  });
};

/*******************************************************************************
 * Poll for link element updates, given by the href, from Emacs.
 */
bdo.poll = function(){
  bdo.log("Polling...");
  $.get(bdo.host + 'poll' + '?reload=' + Math.random(),function(href){
    bdo.refresh(href);
    bdo.poll();
  });
};

/*******************************************************************************
 * Refresh the link with the given `href' attribute.
 */
bdo.refresh = function(href){
  bdo.log("Refreshing '%s'...",href);
  $('link').each(function(){
    if($(this).attr('href').indexOf(href) == 0) {
      // I don't know of any other way to “refresh” an element while
      // preserving ordering such that CSS demands.
      $(this).attr('href',href + "?reload=" + Math.random());
    }
  });
};

/*******************************************************************************
 * Log something if there is a console available.
 */
bdo.log = function(){
  return window.console && window.console.log.apply(window.console,arguments);
}

/*******************************************************************************
 * Below the bdo.host value is filled in and bdo.init is called.
 */
