<!DOCTYPE html>
<html>
<head data-suburl="">
	<meta charset="utf-8">
	<meta http-equiv="x-ua-compatible" content="ie=edge">
	<title>yorhel/vndb: Source code of the Visual Novel Database; https://vndb.org/ - Blicky.net Git Hosting</title>
	<meta name="theme-color" content="#6cc644">
	<meta name="author" content="yorhel" />
	<meta name="description" content="vndb - Source code of the Visual Novel Database; https://vndb.org/" />
	<meta name="keywords" content="go,git,self-hosted,gitea">
	<meta name="referrer" content="no-referrer" />
	<meta name="_csrf" content="yx5SCKQ86jtSZS6YAXjG_vQw-2U6MTUyODA1OTcyMTE4NDcwMTA4Nw==" />
	<meta name="_suburl" content="" />
	
	
	


	<script>
	/*
	@licstart  The following is the entire license notice for the
        JavaScript code in this page.

	Copyright (c) 2016 The Gitea Authors
	Copyright (c) 2015 The Gogs Authors

	Permission is hereby granted, free of charge, to any person obtaining a copy
	of this software and associated documentation files (the "Software"), to deal
	in the Software without restriction, including without limitation the rights
	to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
	copies of the Software, and to permit persons to whom the Software is
	furnished to do so, subject to the following conditions:

	The above copyright notice and this permission notice shall be included in
	all copies or substantial portions of the Software.

	THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
	IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
	FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
	AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
	LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
	OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
	THE SOFTWARE.
	---
	Licensing information for additional javascript libraries can be found at:
	  {{AppSubUrl}}/vendor/librejs.html

	@licend  The above is the entire license notice
        for the JavaScript code in this page.
	*/
	</script>

	<link rel="shortcut icon" href="/img/favicon.png" />
	<link rel="mask-icon" href="/img/gitea-safari.svg" color="#609926">
	<link rel="preload" href="/vendor/assets/font-awesome/css/font-awesome.min.css" as="style" onload="this.rel='stylesheet'">
	<noscript><link rel="stylesheet" href="/vendor/assets/font-awesome/css/font-awesome.min.css"></noscript>
	<link rel="stylesheet" href="/vendor/assets/octicons/octicons.min.css">





	
	<link rel="stylesheet" href="/vendor/plugins/semantic/semantic.min.css">
	<link rel="stylesheet" href="/css/index.css?v=38dfe6388acfa83c3486d41f22b08165">
	<noscript>
		<style>
			.dropdown:hover > .menu { display: block; }
			.ui.secondary.menu .dropdown.item > .menu { margin-top: 0; }
		</style>
	</noscript>


	<link rel="stylesheet" href="/vendor/plugins/highlight/github.css">




	<style class="list-search-style"></style>

	<script src="/vendor/plugins/cssrelpreload/loadCSS.min.js"></script>
	<script src="/vendor/plugins/cssrelpreload/cssrelpreload.min.js"></script>

	<meta property="og:title" content="vndb" />
	<meta property="og:type" content="object" />
	<meta property="og:image" content="https://code.blicky.net/avatars/d898c894d122ac42feefb35cc57ec250" />
	<meta property="og:url" content="https://code.blicky.net/yorhel/vndb" />
	
	<meta property="og:description" content="Source code of the Visual Novel Database; https://vndb.org/" />
	
	<meta property="og:site_name" content="Blicky.net Git Hosting" />

</head>
<body>
	<div class="full height">
		<noscript>This website works better with JavaScript</noscript>

		
			<div class="following bar light">
				<div class="ui container">
					<div class="ui grid">
						<div class="column">
							<div class="ui top secondary menu">
								<a class="item brand" href="/">
									<img class="ui mini image" src="/img/gitea-sm.png">
								</a>

								
									<a class="item" href="/">Home</a>
								

								<a class="item" href="/explore/repos">Explore</a>
								

								

									<a class="item" target="_blank" rel="noopener noreferrer" href="https://docs.gitea.io">Help</a>
									<div class="right menu">
										
											<a class="item" href="/user/sign_up">
												<i class="octicon octicon-person"></i> Register
											</a>
										
										<a class="item" href="/user/login?redirect_to=%2fyorhel%2fvndb%2fsrc%2fbranch%2fmaster%2futil%2fsql%2fschema.sql">
											<i class="octicon octicon-sign-in"></i> Sign In
										</a>
									</div>

								
							</div>
						</div>
					</div>
				</div>
			</div>
		


<div class="repository file list">
	<div class="header-wrapper">

	<div class="ui container">
		<div class="ui vertically padded grid head">
			<div class="column">
				<div class="ui header">
					<div class="ui huge breadcrumb">
						<i class="mega-octicon octicon-repo"></i>
						<a href="/yorhel">yorhel</a>
						<div class="divider"> / </div>
						<a href="/yorhel/vndb">vndb</a>
						
						
					</div>

					<div class="ui right">
						<div class="ui compact labeled button" tabindex="0">
							<a class="ui compact button" href="/yorhel/vndb/action/watch?redirect_to=%2fyorhel%2fvndb%2fsrc%2fbranch%2fmaster%2futil%2fsql%2fschema.sql">
								<i class="icon fa-eye-slash"></i>Watch
							</a>
							<a class="ui basic label" href="/yorhel/vndb/watchers">
								1
							</a>
						</div>
						<div class="ui compact labeled button" tabindex="0">
							<a class="ui compact button" href="/yorhel/vndb/action/star?redirect_to=%2fyorhel%2fvndb%2fsrc%2fbranch%2fmaster%2futil%2fsql%2fschema.sql">
								<i class="icon fa-star-o"></i>Star
							</a>
							<a class="ui basic label" href="/yorhel/vndb/stars">
								1
							</a>
						</div>
						
							<div class="ui compact labeled button" tabindex="0">
								<a class="ui compact button poping up"  data-content="You cannot fork a repository you already own!" data-position="top center" data-variation="tiny">
									<i class="octicon octicon-repo-forked"></i>Fork
								</a>
								<a class="ui basic label" href="/yorhel/vndb/forks">
									0
								</a>
							</div>
						
					</div>
				</div>
			</div>
		</div>
	</div>


	<div class="ui tabs container">
		<div class="ui tabular stackable menu navbar">
			
			<a class="active item" href="/yorhel/vndb">
				<i class="octicon octicon-code"></i> Code
			</a>
			

			
				<a class=" item" href="/yorhel/vndb/issues">
					<i class="octicon octicon-issue-opened"></i> Issues <span class="ui blue small label">1</span>
				</a>
			

			

			
				<a class=" item" href="/yorhel/vndb/pulls">
					<i class="octicon octicon-git-pull-request"></i> Pull Requests <span class="ui gray small label">0</span>
				</a>
			

			
			<a class=" item" href="/yorhel/vndb/releases">
				<i class="octicon octicon-tag"></i> Releases <span class="ui blue small label">38</span>
			</a>
			

			
				<a class=" item" href="/yorhel/vndb/wiki" >
					<i class="octicon octicon-book"></i> Wiki
				</a>
			

			
				<a class=" item" href="/yorhel/vndb/activity">
					<i class="octicon octicon-pulse"></i> Activity
				</a>
			

			
		</div>
	</div>
	<div class="ui tabs divider"></div>

</div>

	<div class="ui container">
		



		<div class="ui repo-description">
			<div id="repo-desc">
				<span class="description has-emoji">Source code of the Visual Novel Database; <a href="https://vndb.org/" target="_blank" rel="noopener">https://vndb.org/</a></span>
				<a class="link" href=""></a>
			</div>
			
		</div>
		<div class="ui segment sub-menu">
	<div class="ui two horizontal center link list">
		
			<div class="item">
				<a href="/yorhel/vndb/commits/branch/master"><i class="octicon octicon-history"></i> <b>2052</b> Commits</a>
			</div>
		
		
			<div class="item">
				<a href="/yorhel/vndb/branches/"><i class="octicon octicon-git-branch"></i> <b>2</b> Branches</a>
			</div>
		
	</div>
</div>

		<div class="ui secondary menu">
			
			<div class="fitted item choose reference">
	<div class="ui floating filter dropdown custom" data-can-create-branch="false" data-no-results="No results found.">
		<div class="ui basic small button" @click="menuVisible = !menuVisible" @keyup.enter="menuVisible = !menuVisible">
			<span class="text">
				<i class="octicon octicon-git-branch"></i>
				Branch:
				<strong>master</strong>
			</span>
			<i class="dropdown icon"></i>
		</div>
		<div class="data" style="display: none" data-mode="branches">
			
				<div class="item branch selected" data-url="/yorhel/vndb/src/branch/master/util/sql/schema.sql">master</div>
			
				<div class="item branch " data-url="/yorhel/vndb/src/branch/v3/util/sql/schema.sql">v3</div>
			
			
				<div class="item tag " data-url="/yorhel/vndb/src/tag/3.0-alpha/util/sql/schema.sql">3.0-alpha</div>
			
				<div class="item tag " data-url="/yorhel/vndb/src/tag/2.26/util/sql/schema.sql">2.26</div>
			
				<div class="item tag " data-url="/yorhel/vndb/src/tag/2.25/util/sql/schema.sql">2.25</div>
			
				<div class="item tag " data-url="/yorhel/vndb/src/tag/2.24/util/sql/schema.sql">2.24</div>
			
				<div class="item tag " data-url="/yorhel/vndb/src/tag/2.23/util/sql/schema.sql">2.23</div>
			
				<div class="item tag " data-url="/yorhel/vndb/src/tag/2.22/util/sql/schema.sql">2.22</div>
			
				<div class="item tag " data-url="/yorhel/vndb/src/tag/2.21/util/sql/schema.sql">2.21</div>
			
				<div class="item tag " data-url="/yorhel/vndb/src/tag/2.20/util/sql/schema.sql">2.20</div>
			
				<div class="item tag " data-url="/yorhel/vndb/src/tag/2.19/util/sql/schema.sql">2.19</div>
			
				<div class="item tag " data-url="/yorhel/vndb/src/tag/2.18/util/sql/schema.sql">2.18</div>
			
				<div class="item tag " data-url="/yorhel/vndb/src/tag/2.17/util/sql/schema.sql">2.17</div>
			
				<div class="item tag " data-url="/yorhel/vndb/src/tag/2.16/util/sql/schema.sql">2.16</div>
			
				<div class="item tag " data-url="/yorhel/vndb/src/tag/2.15/util/sql/schema.sql">2.15</div>
			
				<div class="item tag " data-url="/yorhel/vndb/src/tag/2.14/util/sql/schema.sql">2.14</div>
			
				<div class="item tag " data-url="/yorhel/vndb/src/tag/2.13/util/sql/schema.sql">2.13</div>
			
				<div class="item tag " data-url="/yorhel/vndb/src/tag/2.12/util/sql/schema.sql">2.12</div>
			
				<div class="item tag " data-url="/yorhel/vndb/src/tag/2.11/util/sql/schema.sql">2.11</div>
			
				<div class="item tag " data-url="/yorhel/vndb/src/tag/2.10/util/sql/schema.sql">2.10</div>
			
				<div class="item tag " data-url="/yorhel/vndb/src/tag/2.9/util/sql/schema.sql">2.9</div>
			
				<div class="item tag " data-url="/yorhel/vndb/src/tag/2.8/util/sql/schema.sql">2.8</div>
			
				<div class="item tag " data-url="/yorhel/vndb/src/tag/2.7/util/sql/schema.sql">2.7</div>
			
				<div class="item tag " data-url="/yorhel/vndb/src/tag/2.6/util/sql/schema.sql">2.6</div>
			
				<div class="item tag " data-url="/yorhel/vndb/src/tag/2.5/util/sql/schema.sql">2.5</div>
			
				<div class="item tag " data-url="/yorhel/vndb/src/tag/2.4/util/sql/schema.sql">2.4</div>
			
				<div class="item tag " data-url="/yorhel/vndb/src/tag/2.3/util/sql/schema.sql">2.3</div>
			
				<div class="item tag " data-url="/yorhel/vndb/src/tag/2.2/util/sql/schema.sql">2.2</div>
			
				<div class="item tag " data-url="/yorhel/vndb/src/tag/2.1/util/sql/schema.sql">2.1</div>
			
				<div class="item tag " data-url="/yorhel/vndb/src/tag/2.0/util/sql/schema.sql">2.0</div>
			
				<div class="item tag " data-url="/yorhel/vndb/src/tag/1.23/util/sql/schema.sql">1.23</div>
			
				<div class="item tag " data-url="/yorhel/vndb/src/tag/1.22/util/sql/schema.sql">1.22</div>
			
				<div class="item tag " data-url="/yorhel/vndb/src/tag/1.21/util/sql/schema.sql">1.21</div>
			
				<div class="item tag " data-url="/yorhel/vndb/src/tag/1.20/util/sql/schema.sql">1.20</div>
			
				<div class="item tag " data-url="/yorhel/vndb/src/tag/1.19/util/sql/schema.sql">1.19</div>
			
				<div class="item tag " data-url="/yorhel/vndb/src/tag/1.18/util/sql/schema.sql">1.18</div>
			
				<div class="item tag " data-url="/yorhel/vndb/src/tag/1.17/util/sql/schema.sql">1.17</div>
			
				<div class="item tag " data-url="/yorhel/vndb/src/tag/1.16/util/sql/schema.sql">1.16</div>
			
				<div class="item tag " data-url="/yorhel/vndb/src/tag/1.15/util/sql/schema.sql">1.15</div>
			
				<div class="item tag " data-url="/yorhel/vndb/src/tag/1.14/util/sql/schema.sql">1.14</div>
			
		</div>
		<div class="menu transition" :class="{visible: menuVisible}" v-if="menuVisible" v-cloak>
			<div class="ui icon search input">
				<i class="filter icon"></i>
				<input name="search" ref="searchField" v-model="searchTerm" @keydown="keydown($event)" placeholder="Filter branch or tag...">
			</div>
			<div class="header branch-tag-choice">
				<div class="ui grid">
					<div class="two column row">
						<a class="reference column" href="#" @click="mode = 'branches'; focusSearchField()">
							<span class="text" :class="{black: mode == 'branches'}">
								<i class="octicon octicon-git-branch"></i> Branches
							</span>
						</a>
						<a class="reference column" href="#" @click="mode = 'tags'; focusSearchField()">
							<span class="text" :class="{black: mode == 'tags'}">
								<i class="reference tags icon"></i> Tags
							</span>
						</a>
					</div>
				</div>
			</div>
			<div class="scrolling menu" ref="scrollContainer">
				<div v-for="(item, index) in filteredItems" :key="item.name" class="item" :class="{selected: item.selected, active: active == index}" @click="selectItem(item)" :ref="'listItem' + index">${ item.name }</div>
				<div class="item" v-if="showCreateNewBranch" :class="{active: active == filteredItems.length}" :ref="'listItem' + filteredItems.length">
					<a href="#" @click="createNewBranch()">
						<div>
							<i class="octicon octicon-git-branch"></i>
							Create branch <strong>${ searchTerm }</strong>
						</div>
						<div class="text small">
							
								from 'master'
							
						</div>
					</a>
					<form ref="newBranchForm" action="/yorhel/vndb/branches/_new/branch/master" method="post">
						<input type="hidden" name="_csrf" value="yx5SCKQ86jtSZS6YAXjG_vQw-2U6MTUyODA1OTcyMTE4NDcwMTA4Nw==">
						<input type="hidden" name="new_branch_name" v-model="searchTerm">
					</form>
				</div>
			</div>
			<div class="message" v-if="showNoResults">${ noResults }</div>
		</div>
	</div>
</div>

			
			
			<div class="fitted item"><span class="ui breadcrumb repo-path"><a class="section" href="/yorhel/vndb/src/branch/master">vndb</a><span class="divider">/</span><span class="section"><a href="/yorhel/vndb/src/branch/master/util">util</a></span><span class="divider">/</span><span class="section"><a href="/yorhel/vndb/src/branch/master/util/sql">sql</a></span><span class="divider">/</span><span class="active section">schema.sql</span></span></div>
			<div class="right fitted item">
				
					<div id="file-buttons" class="ui tiny blue buttons">
						
						
					</div>
				

				
				
			</div>
		</div>
		
			<div class="tab-size-8 non-diff-file-content">
	<h4 class="ui top attached header" id="repo-read-file">
		
			<i class="file text outline icon ui left"></i>
			<strong>schema.sql</strong> <span class="text grey normal">22KB</span>
		
		
			<div class="ui right file-actions">
				<div class="ui buttons">
					
						<a class="ui button" href="/yorhel/vndb/src/commit/0c0007630fd310bc6857a94fe65703d1fb446177/util/sql/schema.sql">Permalink</a>
					
					<a class="ui button" href="/yorhel/vndb/commits/branch/master/util/sql/schema.sql">History</a>
					<a class="ui button" href="/yorhel/vndb/raw/branch/master/util/sql/schema.sql">Raw</a>
				</div>
				
					
						<i class="octicon octicon-pencil btn-octicon poping up disabled" data-content="You must fork this repository before editing the file" data-position="bottom center" data-variation="tiny inverted"></i>
					
					
						<i class="octicon octicon-trashcan btn-octicon poping up disabled" data-content="You must have write access to make or propose changes to this file" data-position="bottom center" data-variation="tiny inverted"></i>
					
				
			</div>
		
	</h4>
	<div class="ui attached table segment">
		<div class="file-view code-view has-emoji">
			
				<table>
					<tbody>
						<tr>
						
							<td class="lines-num"><span id="L1">1</span><span id="L2">2</span><span id="L3">3</span><span id="L4">4</span><span id="L5">5</span><span id="L6">6</span><span id="L7">7</span><span id="L8">8</span><span id="L9">9</span><span id="L10">10</span><span id="L11">11</span><span id="L12">12</span><span id="L13">13</span><span id="L14">14</span><span id="L15">15</span><span id="L16">16</span><span id="L17">17</span><span id="L18">18</span><span id="L19">19</span><span id="L20">20</span><span id="L21">21</span><span id="L22">22</span><span id="L23">23</span><span id="L24">24</span><span id="L25">25</span><span id="L26">26</span><span id="L27">27</span><span id="L28">28</span><span id="L29">29</span><span id="L30">30</span><span id="L31">31</span><span id="L32">32</span><span id="L33">33</span><span id="L34">34</span><span id="L35">35</span><span id="L36">36</span><span id="L37">37</span><span id="L38">38</span><span id="L39">39</span><span id="L40">40</span><span id="L41">41</span><span id="L42">42</span><span id="L43">43</span><span id="L44">44</span><span id="L45">45</span><span id="L46">46</span><span id="L47">47</span><span id="L48">48</span><span id="L49">49</span><span id="L50">50</span><span id="L51">51</span><span id="L52">52</span><span id="L53">53</span><span id="L54">54</span><span id="L55">55</span><span id="L56">56</span><span id="L57">57</span><span id="L58">58</span><span id="L59">59</span><span id="L60">60</span><span id="L61">61</span><span id="L62">62</span><span id="L63">63</span><span id="L64">64</span><span id="L65">65</span><span id="L66">66</span><span id="L67">67</span><span id="L68">68</span><span id="L69">69</span><span id="L70">70</span><span id="L71">71</span><span id="L72">72</span><span id="L73">73</span><span id="L74">74</span><span id="L75">75</span><span id="L76">76</span><span id="L77">77</span><span id="L78">78</span><span id="L79">79</span><span id="L80">80</span><span id="L81">81</span><span id="L82">82</span><span id="L83">83</span><span id="L84">84</span><span id="L85">85</span><span id="L86">86</span><span id="L87">87</span><span id="L88">88</span><span id="L89">89</span><span id="L90">90</span><span id="L91">91</span><span id="L92">92</span><span id="L93">93</span><span id="L94">94</span><span id="L95">95</span><span id="L96">96</span><span id="L97">97</span><span id="L98">98</span><span id="L99">99</span><span id="L100">100</span><span id="L101">101</span><span id="L102">102</span><span id="L103">103</span><span id="L104">104</span><span id="L105">105</span><span id="L106">106</span><span id="L107">107</span><span id="L108">108</span><span id="L109">109</span><span id="L110">110</span><span id="L111">111</span><span id="L112">112</span><span id="L113">113</span><span id="L114">114</span><span id="L115">115</span><span id="L116">116</span><span id="L117">117</span><span id="L118">118</span><span id="L119">119</span><span id="L120">120</span><span id="L121">121</span><span id="L122">122</span><span id="L123">123</span><span id="L124">124</span><span id="L125">125</span><span id="L126">126</span><span id="L127">127</span><span id="L128">128</span><span id="L129">129</span><span id="L130">130</span><span id="L131">131</span><span id="L132">132</span><span id="L133">133</span><span id="L134">134</span><span id="L135">135</span><span id="L136">136</span><span id="L137">137</span><span id="L138">138</span><span id="L139">139</span><span id="L140">140</span><span id="L141">141</span><span id="L142">142</span><span id="L143">143</span><span id="L144">144</span><span id="L145">145</span><span id="L146">146</span><span id="L147">147</span><span id="L148">148</span><span id="L149">149</span><span id="L150">150</span><span id="L151">151</span><span id="L152">152</span><span id="L153">153</span><span id="L154">154</span><span id="L155">155</span><span id="L156">156</span><span id="L157">157</span><span id="L158">158</span><span id="L159">159</span><span id="L160">160</span><span id="L161">161</span><span id="L162">162</span><span id="L163">163</span><span id="L164">164</span><span id="L165">165</span><span id="L166">166</span><span id="L167">167</span><span id="L168">168</span><span id="L169">169</span><span id="L170">170</span><span id="L171">171</span><span id="L172">172</span><span id="L173">173</span><span id="L174">174</span><span id="L175">175</span><span id="L176">176</span><span id="L177">177</span><span id="L178">178</span><span id="L179">179</span><span id="L180">180</span><span id="L181">181</span><span id="L182">182</span><span id="L183">183</span><span id="L184">184</span><span id="L185">185</span><span id="L186">186</span><span id="L187">187</span><span id="L188">188</span><span id="L189">189</span><span id="L190">190</span><span id="L191">191</span><span id="L192">192</span><span id="L193">193</span><span id="L194">194</span><span id="L195">195</span><span id="L196">196</span><span id="L197">197</span><span id="L198">198</span><span id="L199">199</span><span id="L200">200</span><span id="L201">201</span><span id="L202">202</span><span id="L203">203</span><span id="L204">204</span><span id="L205">205</span><span id="L206">206</span><span id="L207">207</span><span id="L208">208</span><span id="L209">209</span><span id="L210">210</span><span id="L211">211</span><span id="L212">212</span><span id="L213">213</span><span id="L214">214</span><span id="L215">215</span><span id="L216">216</span><span id="L217">217</span><span id="L218">218</span><span id="L219">219</span><span id="L220">220</span><span id="L221">221</span><span id="L222">222</span><span id="L223">223</span><span id="L224">224</span><span id="L225">225</span><span id="L226">226</span><span id="L227">227</span><span id="L228">228</span><span id="L229">229</span><span id="L230">230</span><span id="L231">231</span><span id="L232">232</span><span id="L233">233</span><span id="L234">234</span><span id="L235">235</span><span id="L236">236</span><span id="L237">237</span><span id="L238">238</span><span id="L239">239</span><span id="L240">240</span><span id="L241">241</span><span id="L242">242</span><span id="L243">243</span><span id="L244">244</span><span id="L245">245</span><span id="L246">246</span><span id="L247">247</span><span id="L248">248</span><span id="L249">249</span><span id="L250">250</span><span id="L251">251</span><span id="L252">252</span><span id="L253">253</span><span id="L254">254</span><span id="L255">255</span><span id="L256">256</span><span id="L257">257</span><span id="L258">258</span><span id="L259">259</span><span id="L260">260</span><span id="L261">261</span><span id="L262">262</span><span id="L263">263</span><span id="L264">264</span><span id="L265">265</span><span id="L266">266</span><span id="L267">267</span><span id="L268">268</span><span id="L269">269</span><span id="L270">270</span><span id="L271">271</span><span id="L272">272</span><span id="L273">273</span><span id="L274">274</span><span id="L275">275</span><span id="L276">276</span><span id="L277">277</span><span id="L278">278</span><span id="L279">279</span><span id="L280">280</span><span id="L281">281</span><span id="L282">282</span><span id="L283">283</span><span id="L284">284</span><span id="L285">285</span><span id="L286">286</span><span id="L287">287</span><span id="L288">288</span><span id="L289">289</span><span id="L290">290</span><span id="L291">291</span><span id="L292">292</span><span id="L293">293</span><span id="L294">294</span><span id="L295">295</span><span id="L296">296</span><span id="L297">297</span><span id="L298">298</span><span id="L299">299</span><span id="L300">300</span><span id="L301">301</span><span id="L302">302</span><span id="L303">303</span><span id="L304">304</span><span id="L305">305</span><span id="L306">306</span><span id="L307">307</span><span id="L308">308</span><span id="L309">309</span><span id="L310">310</span><span id="L311">311</span><span id="L312">312</span><span id="L313">313</span><span id="L314">314</span><span id="L315">315</span><span id="L316">316</span><span id="L317">317</span><span id="L318">318</span><span id="L319">319</span><span id="L320">320</span><span id="L321">321</span><span id="L322">322</span><span id="L323">323</span><span id="L324">324</span><span id="L325">325</span><span id="L326">326</span><span id="L327">327</span><span id="L328">328</span><span id="L329">329</span><span id="L330">330</span><span id="L331">331</span><span id="L332">332</span><span id="L333">333</span><span id="L334">334</span><span id="L335">335</span><span id="L336">336</span><span id="L337">337</span><span id="L338">338</span><span id="L339">339</span><span id="L340">340</span><span id="L341">341</span><span id="L342">342</span><span id="L343">343</span><span id="L344">344</span><span id="L345">345</span><span id="L346">346</span><span id="L347">347</span><span id="L348">348</span><span id="L349">349</span><span id="L350">350</span><span id="L351">351</span><span id="L352">352</span><span id="L353">353</span><span id="L354">354</span><span id="L355">355</span><span id="L356">356</span><span id="L357">357</span><span id="L358">358</span><span id="L359">359</span><span id="L360">360</span><span id="L361">361</span><span id="L362">362</span><span id="L363">363</span><span id="L364">364</span><span id="L365">365</span><span id="L366">366</span><span id="L367">367</span><span id="L368">368</span><span id="L369">369</span><span id="L370">370</span><span id="L371">371</span><span id="L372">372</span><span id="L373">373</span><span id="L374">374</span><span id="L375">375</span><span id="L376">376</span><span id="L377">377</span><span id="L378">378</span><span id="L379">379</span><span id="L380">380</span><span id="L381">381</span><span id="L382">382</span><span id="L383">383</span><span id="L384">384</span><span id="L385">385</span><span id="L386">386</span><span id="L387">387</span><span id="L388">388</span><span id="L389">389</span><span id="L390">390</span><span id="L391">391</span><span id="L392">392</span><span id="L393">393</span><span id="L394">394</span><span id="L395">395</span><span id="L396">396</span><span id="L397">397</span><span id="L398">398</span><span id="L399">399</span><span id="L400">400</span><span id="L401">401</span><span id="L402">402</span><span id="L403">403</span><span id="L404">404</span><span id="L405">405</span><span id="L406">406</span><span id="L407">407</span><span id="L408">408</span><span id="L409">409</span><span id="L410">410</span><span id="L411">411</span><span id="L412">412</span><span id="L413">413</span><span id="L414">414</span><span id="L415">415</span><span id="L416">416</span><span id="L417">417</span><span id="L418">418</span><span id="L419">419</span><span id="L420">420</span><span id="L421">421</span><span id="L422">422</span><span id="L423">423</span><span id="L424">424</span><span id="L425">425</span><span id="L426">426</span><span id="L427">427</span><span id="L428">428</span><span id="L429">429</span><span id="L430">430</span><span id="L431">431</span><span id="L432">432</span><span id="L433">433</span><span id="L434">434</span><span id="L435">435</span><span id="L436">436</span><span id="L437">437</span><span id="L438">438</span><span id="L439">439</span><span id="L440">440</span><span id="L441">441</span><span id="L442">442</span><span id="L443">443</span><span id="L444">444</span><span id="L445">445</span><span id="L446">446</span><span id="L447">447</span><span id="L448">448</span><span id="L449">449</span><span id="L450">450</span><span id="L451">451</span><span id="L452">452</span><span id="L453">453</span><span id="L454">454</span><span id="L455">455</span><span id="L456">456</span><span id="L457">457</span><span id="L458">458</span><span id="L459">459</span><span id="L460">460</span><span id="L461">461</span><span id="L462">462</span><span id="L463">463</span><span id="L464">464</span><span id="L465">465</span><span id="L466">466</span><span id="L467">467</span><span id="L468">468</span><span id="L469">469</span><span id="L470">470</span><span id="L471">471</span><span id="L472">472</span><span id="L473">473</span><span id="L474">474</span><span id="L475">475</span><span id="L476">476</span><span id="L477">477</span><span id="L478">478</span><span id="L479">479</span><span id="L480">480</span><span id="L481">481</span><span id="L482">482</span><span id="L483">483</span><span id="L484">484</span><span id="L485">485</span><span id="L486">486</span><span id="L487">487</span><span id="L488">488</span><span id="L489">489</span><span id="L490">490</span><span id="L491">491</span><span id="L492">492</span><span id="L493">493</span><span id="L494">494</span><span id="L495">495</span><span id="L496">496</span><span id="L497">497</span><span id="L498">498</span><span id="L499">499</span><span id="L500">500</span><span id="L501">501</span><span id="L502">502</span><span id="L503">503</span><span id="L504">504</span><span id="L505">505</span><span id="L506">506</span><span id="L507">507</span><span id="L508">508</span><span id="L509">509</span><span id="L510">510</span><span id="L511">511</span><span id="L512">512</span><span id="L513">513</span><span id="L514">514</span><span id="L515">515</span><span id="L516">516</span><span id="L517">517</span><span id="L518">518</span><span id="L519">519</span><span id="L520">520</span><span id="L521">521</span><span id="L522">522</span><span id="L523">523</span><span id="L524">524</span><span id="L525">525</span><span id="L526">526</span><span id="L527">527</span><span id="L528">528</span><span id="L529">529</span><span id="L530">530</span><span id="L531">531</span><span id="L532">532</span><span id="L533">533</span><span id="L534">534</span><span id="L535">535</span><span id="L536">536</span><span id="L537">537</span><span id="L538">538</span><span id="L539">539</span><span id="L540">540</span><span id="L541">541</span><span id="L542">542</span><span id="L543">543</span><span id="L544">544</span><span id="L545">545</span><span id="L546">546</span><span id="L547">547</span><span id="L548">548</span><span id="L549">549</span><span id="L550">550</span><span id="L551">551</span><span id="L552">552</span><span id="L553">553</span><span id="L554">554</span><span id="L555">555</span><span id="L556">556</span><span id="L557">557</span><span id="L558">558</span><span id="L559">559</span><span id="L560">560</span><span id="L561">561</span><span id="L562">562</span><span id="L563">563</span><span id="L564">564</span><span id="L565">565</span><span id="L566">566</span><span id="L567">567</span><span id="L568">568</span><span id="L569">569</span><span id="L570">570</span><span id="L571">571</span><span id="L572">572</span><span id="L573">573</span><span id="L574">574</span><span id="L575">575</span><span id="L576">576</span><span id="L577">577</span><span id="L578">578</span><span id="L579">579</span><span id="L580">580</span><span id="L581">581</span><span id="L582">582</span><span id="L583">583</span><span id="L584">584</span><span id="L585">585</span><span id="L586">586</span><span id="L587">587</span><span id="L588">588</span><span id="L589">589</span><span id="L590">590</span><span id="L591">591</span><span id="L592">592</span><span id="L593">593</span><span id="L594">594</span><span id="L595">595</span><span id="L596">596</span><span id="L597">597</span><span id="L598">598</span><span id="L599">599</span><span id="L600">600</span><span id="L601">601</span><span id="L602">602</span><span id="L603">603</span><span id="L604">604</span><span id="L605">605</span><span id="L606">606</span><span id="L607">607</span><span id="L608">608</span><span id="L609">609</span><span id="L610">610</span><span id="L611">611</span><span id="L612">612</span><span id="L613">613</span><span id="L614">614</span><span id="L615">615</span><span id="L616">616</span><span id="L617">617</span><span id="L618">618</span><span id="L619">619</span><span id="L620">620</span><span id="L621">621</span><span id="L622">622</span><span id="L623">623</span><span id="L624">624</span><span id="L625">625</span><span id="L626">626</span><span id="L627">627</span><span id="L628">628</span><span id="L629">629</span><span id="L630">630</span><span id="L631">631</span><span id="L632">632</span><span id="L633">633</span><span id="L634">634</span><span id="L635">635</span><span id="L636">636</span><span id="L637">637</span><span id="L638">638</span><span id="L639">639</span><span id="L640">640</span><span id="L641">641</span><span id="L642">642</span><span id="L643">643</span><span id="L644">644</span><span id="L645">645</span><span id="L646">646</span><span id="L647">647</span><span id="L648">648</span><span id="L649">649</span><span id="L650">650</span><span id="L651">651</span><span id="L652">652</span><span id="L653">653</span><span id="L654">654</span><span id="L655">655</span><span id="L656">656</span><span id="L657">657</span><span id="L658">658</span><span id="L659">659</span><span id="L660">660</span><span id="L661">661</span><span id="L662">662</span><span id="L663">663</span><span id="L664">664</span><span id="L665">665</span><span id="L666">666</span><span id="L667">667</span><span id="L668">668</span><span id="L669">669</span><span id="L670">670</span><span id="L671">671</span><span id="L672">672</span><span id="L673">673</span><span id="L674">674</span><span id="L675">675</span><span id="L676">676</span><span id="L677">677</span><span id="L678">678</span><span id="L679">679</span><span id="L680">680</span><span id="L681">681</span><span id="L682">682</span><span id="L683">683</span><span id="L684">684</span><span id="L685">685</span><span id="L686">686</span><span id="L687">687</span><span id="L688">688</span><span id="L689">689</span><span id="L690">690</span><span id="L691">691</span><span id="L692">692</span><span id="L693">693</span><span id="L694">694</span><span id="L695">695</span><span id="L696">696</span><span id="L697">697</span><span id="L698">698</span><span id="L699">699</span><span id="L700">700</span><span id="L701">701</span><span id="L702">702</span><span id="L703">703</span><span id="L704">704</span><span id="L705">705</span><span id="L706">706</span><span id="L707">707</span><span id="L708">708</span><span id="L709">709</span><span id="L710">710</span><span id="L711">711</span><span id="L712">712</span><span id="L713">713</span><span id="L714">714</span><span id="L715">715</span><span id="L716">716</span><span id="L717">717</span><span id="L718">718</span><span id="L719">719</span><span id="L720">720</span><span id="L721">721</span><span id="L722">722</span><span id="L723">723</span><span id="L724">724</span><span id="L725">725</span><span id="L726">726</span><span id="L727">727</span><span id="L728">728</span><span id="L729">729</span><span id="L730">730</span><span id="L731">731</span><span id="L732">732</span><span id="L733">733</span><span id="L734">734</span><span id="L735">735</span><span id="L736">736</span><span id="L737">737</span><span id="L738">738</span><span id="L739">739</span><span id="L740">740</span><span id="L741">741</span><span id="L742">742</span><span id="L743">743</span><span id="L744">744</span><span id="L745">745</span><span id="L746">746</span><span id="L747">747</span><span id="L748">748</span><span id="L749">749</span><span id="L750">750</span><span id="L751">751</span><span id="L752">752</span><span id="L753">753</span><span id="L754">754</span><span id="L755">755</span><span id="L756">756</span><span id="L757">757</span><span id="L758">758</span><span id="L759">759</span><span id="L760">760</span><span id="L761">761</span><span id="L762">762</span><span id="L763">763</span><span id="L764">764</span><span id="L765">765</span><span id="L766">766</span><span id="L767">767</span><span id="L768">768</span><span id="L769">769</span><span id="L770">770</span></td>
							<td class="lines-code"><pre><code class="sql"><ol class="linenums"><li class="L1" rel="L1">-- Convention for database items with version control:
</li><li class="L2" rel="L2">--
</li><li class="L3" rel="L3">--   CREATE TABLE items ( -- dbentry_type=x
</li><li class="L4" rel="L4">--     id        SERIAL PRIMARY KEY,
</li><li class="L5" rel="L5">--     locked    boolean NOT NULL DEFAULT FALSE,
</li><li class="L6" rel="L6">--     hidden    boolean NOT NULL DEFAULT FALSE,
</li><li class="L7" rel="L7">--     -- item-specific columns here
</li><li class="L8" rel="L8">--   );
</li><li class="L9" rel="L9">--   CREATE TABLE items_hist ( -- History of the &#39;items&#39; table
</li><li class="L10" rel="L10">--     chid integer NOT NULL,  -- references changes.id
</li><li class="L11" rel="L11">--     -- item-specific columns here
</li><li class="L12" rel="L12">--   );
</li><li class="L13" rel="L13">--
</li><li class="L14" rel="L14">-- The &#39;-- dbentry_type=x&#39; comment is required, and is used by
</li><li class="L15" rel="L15">-- util/sqleditfunc.pl to generate the correct editing functions.  The history
</li><li class="L16" rel="L16">-- of the &#39;locked&#39; and &#39;hidden&#39; flags is recorded in the changes table.  It&#39;s
</li><li class="L17" rel="L17">-- possible for &#39;items&#39; to have more item-specific columns than &#39;items_hist&#39;.
</li><li class="L18" rel="L18">-- Some columns are caches or otherwise autogenerated, and do not need to be
</li><li class="L19" rel="L19">-- versioned.
</li><li class="L20" rel="L20">--
</li><li class="L21" rel="L21">-- item-related tables work roughly the same:
</li><li class="L22" rel="L22">--
</li><li class="L23" rel="L23">--   CREATE TABLE items_field (
</li><li class="L24" rel="L24">--     id integer,  -- references items.id
</li><li class="L25" rel="L25">--     -- field-specific columns here
</li><li class="L26" rel="L26">--   );
</li><li class="L27" rel="L27">--   CREATE TABLE items_field_hist ( -- History of the &#39;items_field&#39; table
</li><li class="L28" rel="L28">--     chid integer, -- references changes.id
</li><li class="L29" rel="L29">--     -- field-specific columns here
</li><li class="L30" rel="L30">--   );
</li><li class="L31" rel="L31">--
</li><li class="L32" rel="L32">-- The changes and *_hist tables contain all the data. In a sense, the other
</li><li class="L33" rel="L33">-- tables related to the item are just a cache/view into the latest versions.
</li><li class="L34" rel="L34">-- All modifications to the item tables has to go through the edit_* functions
</li><li class="L35" rel="L35">-- in func.sql, these are also responsible for keeping things synchronized.
</li><li class="L36" rel="L36">--
</li><li class="L37" rel="L37">-- Note: Every CREATE TABLE clause and each column should be on a separate
</li><li class="L38" rel="L38">-- line. This file is parsed by util/sqleditfunc.pl, and it doesn&#39;t implement a
</li><li class="L39" rel="L39">-- full SQL query parser.
</li><li class="L40" rel="L40">
</li><li class="L41" rel="L41">
</li><li class="L42" rel="L42">-- affiliate_links
</li><li class="L43" rel="L43">CREATE TABLE affiliate_links (
</li><li class="L44" rel="L44">  id SERIAL PRIMARY KEY,
</li><li class="L45" rel="L45">  rid integer NOT NULL,
</li><li class="L46" rel="L46">  hidden boolean NOT NULL DEFAULT false,
</li><li class="L47" rel="L47">  priority smallint NOT NULL DEFAULT 0,
</li><li class="L48" rel="L48">  affiliate smallint NOT NULL DEFAULT 0,
</li><li class="L49" rel="L49">  url varchar NOT NULL,
</li><li class="L50" rel="L50">  version varchar NOT NULL DEFAULT &#39;&#39;,
</li><li class="L51" rel="L51">  lastfetch timestamptz,
</li><li class="L52" rel="L52">  price varchar NOT NULL DEFAULT &#39;&#39;,
</li><li class="L53" rel="L53">  data varchar NOT NULL DEFAULT &#39;&#39;
</li><li class="L54" rel="L54">);
</li><li class="L55" rel="L55">
</li><li class="L56" rel="L56">-- anime
</li><li class="L57" rel="L57">CREATE TABLE anime (
</li><li class="L58" rel="L58">  id integer NOT NULL PRIMARY KEY,
</li><li class="L59" rel="L59">  year smallint,
</li><li class="L60" rel="L60">  ann_id integer,
</li><li class="L61" rel="L61">  nfo_id varchar(200),
</li><li class="L62" rel="L62">  type anime_type,
</li><li class="L63" rel="L63">  title_romaji varchar(250),
</li><li class="L64" rel="L64">  title_kanji varchar(250),
</li><li class="L65" rel="L65">  lastfetch timestamptz
</li><li class="L66" rel="L66">);
</li><li class="L67" rel="L67">
</li><li class="L68" rel="L68">-- changes
</li><li class="L69" rel="L69">CREATE TABLE changes (
</li><li class="L70" rel="L70">  id         SERIAL PRIMARY KEY,
</li><li class="L71" rel="L71">  type       dbentry_type NOT NULL,
</li><li class="L72" rel="L72">  itemid     integer NOT NULL,
</li><li class="L73" rel="L73">  rev        integer NOT NULL DEFAULT 1,
</li><li class="L74" rel="L74">  added      timestamptz NOT NULL DEFAULT NOW(),
</li><li class="L75" rel="L75">  requester  integer NOT NULL DEFAULT 0,
</li><li class="L76" rel="L76">  ip         inet NOT NULL DEFAULT &#39;0.0.0.0&#39;,
</li><li class="L77" rel="L77">  comments   text NOT NULL DEFAULT &#39;&#39;,
</li><li class="L78" rel="L78">  ihid       boolean NOT NULL DEFAULT FALSE,
</li><li class="L79" rel="L79">  ilock      boolean NOT NULL DEFAULT FALSE
</li><li class="L80" rel="L80">);
</li><li class="L81" rel="L81">
</li><li class="L82" rel="L82">-- chars
</li><li class="L83" rel="L83">CREATE TABLE chars ( -- dbentry_type=c
</li><li class="L84" rel="L84">  id         SERIAL PRIMARY KEY,
</li><li class="L85" rel="L85">  locked     boolean NOT NULL DEFAULT FALSE,
</li><li class="L86" rel="L86">  hidden     boolean NOT NULL DEFAULT FALSE,
</li><li class="L87" rel="L87">  name       varchar(250) NOT NULL DEFAULT &#39;&#39;,
</li><li class="L88" rel="L88">  original   varchar(250) NOT NULL DEFAULT &#39;&#39;,
</li><li class="L89" rel="L89">  alias      varchar(500) NOT NULL DEFAULT &#39;&#39;,
</li><li class="L90" rel="L90">  image      integer  NOT NULL DEFAULT 0,
</li><li class="L91" rel="L91">  &#34;desc&#34;     text     NOT NULL DEFAULT &#39;&#39;,
</li><li class="L92" rel="L92">  gender     gender NOT NULL DEFAULT &#39;unknown&#39;,
</li><li class="L93" rel="L93">  s_bust     smallint NOT NULL DEFAULT 0,
</li><li class="L94" rel="L94">  s_waist    smallint NOT NULL DEFAULT 0,
</li><li class="L95" rel="L95">  s_hip      smallint NOT NULL DEFAULT 0,
</li><li class="L96" rel="L96">  b_month    smallint NOT NULL DEFAULT 0,
</li><li class="L97" rel="L97">  b_day      smallint NOT NULL DEFAULT 0,
</li><li class="L98" rel="L98">  height     smallint NOT NULL DEFAULT 0,
</li><li class="L99" rel="L99">  weight     smallint NOT NULL DEFAULT 0,
</li><li class="L100" rel="L100">  bloodt     blood_type NOT NULL DEFAULT &#39;unknown&#39;,
</li><li class="L101" rel="L101">  main       integer, -- chars.id
</li><li class="L102" rel="L102">  main_spoil smallint NOT NULL DEFAULT 0
</li><li class="L103" rel="L103">);
</li><li class="L104" rel="L104">
</li><li class="L105" rel="L105">-- chars_hist
</li><li class="L106" rel="L106">CREATE TABLE chars_hist (
</li><li class="L107" rel="L107">  chid       integer  NOT NULL PRIMARY KEY,
</li><li class="L108" rel="L108">  name       varchar(250) NOT NULL DEFAULT &#39;&#39;,
</li><li class="L109" rel="L109">  original   varchar(250) NOT NULL DEFAULT &#39;&#39;,
</li><li class="L110" rel="L110">  alias      varchar(500) NOT NULL DEFAULT &#39;&#39;,
</li><li class="L111" rel="L111">  image      integer  NOT NULL DEFAULT 0,
</li><li class="L112" rel="L112">  &#34;desc&#34;     text     NOT NULL DEFAULT &#39;&#39;,
</li><li class="L113" rel="L113">  gender     gender NOT NULL DEFAULT &#39;unknown&#39;,
</li><li class="L114" rel="L114">  s_bust     smallint NOT NULL DEFAULT 0,
</li><li class="L115" rel="L115">  s_waist    smallint NOT NULL DEFAULT 0,
</li><li class="L116" rel="L116">  s_hip      smallint NOT NULL DEFAULT 0,
</li><li class="L117" rel="L117">  b_month    smallint NOT NULL DEFAULT 0,
</li><li class="L118" rel="L118">  b_day      smallint NOT NULL DEFAULT 0,
</li><li class="L119" rel="L119">  height     smallint NOT NULL DEFAULT 0,
</li><li class="L120" rel="L120">  weight     smallint NOT NULL DEFAULT 0,
</li><li class="L121" rel="L121">  bloodt     blood_type NOT NULL DEFAULT &#39;unknown&#39;,
</li><li class="L122" rel="L122">  main       integer, -- chars.id
</li><li class="L123" rel="L123">  main_spoil smallint NOT NULL DEFAULT 0
</li><li class="L124" rel="L124">);
</li><li class="L125" rel="L125">
</li><li class="L126" rel="L126">-- chars_traits
</li><li class="L127" rel="L127">CREATE TABLE chars_traits (
</li><li class="L128" rel="L128">  id         integer NOT NULL,
</li><li class="L129" rel="L129">  tid        integer NOT NULL, -- traits.id
</li><li class="L130" rel="L130">  spoil      smallint NOT NULL DEFAULT 0,
</li><li class="L131" rel="L131">  PRIMARY KEY(id, tid)
</li><li class="L132" rel="L132">);
</li><li class="L133" rel="L133">
</li><li class="L134" rel="L134">-- chars_traits_hist
</li><li class="L135" rel="L135">CREATE TABLE chars_traits_hist (
</li><li class="L136" rel="L136">  chid       integer NOT NULL,
</li><li class="L137" rel="L137">  tid        integer NOT NULL, -- traits.id
</li><li class="L138" rel="L138">  spoil      smallint NOT NULL DEFAULT 0,
</li><li class="L139" rel="L139">  PRIMARY KEY(chid, tid)
</li><li class="L140" rel="L140">);
</li><li class="L141" rel="L141">
</li><li class="L142" rel="L142">-- chars_vns
</li><li class="L143" rel="L143">CREATE TABLE chars_vns (
</li><li class="L144" rel="L144">  id         integer NOT NULL,
</li><li class="L145" rel="L145">  vid        integer NOT NULL, -- vn.id
</li><li class="L146" rel="L146">  rid        integer NULL, -- releases.id
</li><li class="L147" rel="L147">  spoil      smallint NOT NULL DEFAULT 0,
</li><li class="L148" rel="L148">  role       char_role NOT NULL DEFAULT &#39;main&#39;
</li><li class="L149" rel="L149">);
</li><li class="L150" rel="L150">
</li><li class="L151" rel="L151">-- chars_vns_hist
</li><li class="L152" rel="L152">CREATE TABLE chars_vns_hist (
</li><li class="L153" rel="L153">  chid       integer NOT NULL,
</li><li class="L154" rel="L154">  vid        integer NOT NULL, -- vn.id
</li><li class="L155" rel="L155">  rid        integer NULL, -- releases.id
</li><li class="L156" rel="L156">  spoil      smallint NOT NULL DEFAULT 0,
</li><li class="L157" rel="L157">  role       char_role NOT NULL DEFAULT &#39;main&#39;
</li><li class="L158" rel="L158">);
</li><li class="L159" rel="L159">
</li><li class="L160" rel="L160">-- docs
</li><li class="L161" rel="L161">CREATE TABLE docs ( -- dbentry_type=d
</li><li class="L162" rel="L162">  id         SERIAL PRIMARY KEY,
</li><li class="L163" rel="L163">  locked     boolean NOT NULL DEFAULT FALSE,
</li><li class="L164" rel="L164">  hidden     boolean NOT NULL DEFAULT FALSE,
</li><li class="L165" rel="L165">  title      varchar(200) NOT NULL DEFAULT &#39;&#39;,
</li><li class="L166" rel="L166">  content    text NOT NULL DEFAULT &#39;&#39;
</li><li class="L167" rel="L167">);
</li><li class="L168" rel="L168">
</li><li class="L169" rel="L169">-- docs_hist
</li><li class="L170" rel="L170">CREATE TABLE docs_hist (
</li><li class="L171" rel="L171">  chid       integer  NOT NULL PRIMARY KEY,
</li><li class="L172" rel="L172">  title      varchar(200) NOT NULL DEFAULT &#39;&#39;,
</li><li class="L173" rel="L173">  content    text NOT NULL DEFAULT &#39;&#39;
</li><li class="L174" rel="L174">);
</li><li class="L175" rel="L175">
</li><li class="L176" rel="L176">-- login_throttle
</li><li class="L177" rel="L177">CREATE TABLE login_throttle (
</li><li class="L178" rel="L178">  ip inet NOT NULL PRIMARY KEY,
</li><li class="L179" rel="L179">  timeout timestamptz NOT NULL
</li><li class="L180" rel="L180">);
</li><li class="L181" rel="L181">
</li><li class="L182" rel="L182">-- notifications
</li><li class="L183" rel="L183">CREATE TABLE notifications (
</li><li class="L184" rel="L184">  id serial PRIMARY KEY,
</li><li class="L185" rel="L185">  uid integer NOT NULL,
</li><li class="L186" rel="L186">  date timestamptz NOT NULL DEFAULT NOW(),
</li><li class="L187" rel="L187">  read timestamptz,
</li><li class="L188" rel="L188">  ntype notification_ntype NOT NULL,
</li><li class="L189" rel="L189">  ltype notification_ltype NOT NULL,
</li><li class="L190" rel="L190">  iid integer NOT NULL,
</li><li class="L191" rel="L191">  subid integer,
</li><li class="L192" rel="L192">  c_title text NOT NULL,
</li><li class="L193" rel="L193">  c_byuser integer NOT NULL DEFAULT 0
</li><li class="L194" rel="L194">);
</li><li class="L195" rel="L195">
</li><li class="L196" rel="L196">-- producers
</li><li class="L197" rel="L197">CREATE TABLE producers ( -- dbentry_type=p
</li><li class="L198" rel="L198">  id         SERIAL PRIMARY KEY,
</li><li class="L199" rel="L199">  locked     boolean NOT NULL DEFAULT FALSE,
</li><li class="L200" rel="L200">  hidden     boolean NOT NULL DEFAULT FALSE,
</li><li class="L201" rel="L201">  type       producer_type NOT NULL DEFAULT &#39;co&#39;,
</li><li class="L202" rel="L202">  name       varchar(200) NOT NULL DEFAULT &#39;&#39;,
</li><li class="L203" rel="L203">  original   varchar(200) NOT NULL DEFAULT &#39;&#39;,
</li><li class="L204" rel="L204">  website    varchar(250) NOT NULL DEFAULT &#39;&#39;,
</li><li class="L205" rel="L205">  lang       language NOT NULL DEFAULT &#39;ja&#39;,
</li><li class="L206" rel="L206">  &#34;desc&#34;     text NOT NULL DEFAULT &#39;&#39;,
</li><li class="L207" rel="L207">  alias      varchar(500) NOT NULL DEFAULT &#39;&#39;,
</li><li class="L208" rel="L208">  l_wp       varchar(150),
</li><li class="L209" rel="L209">  rgraph     integer -- relgraphs.id
</li><li class="L210" rel="L210">);
</li><li class="L211" rel="L211">
</li><li class="L212" rel="L212">-- producers_hist
</li><li class="L213" rel="L213">CREATE TABLE producers_hist (
</li><li class="L214" rel="L214">  chid       integer NOT NULL PRIMARY KEY,
</li><li class="L215" rel="L215">  type       producer_type NOT NULL DEFAULT &#39;co&#39;,
</li><li class="L216" rel="L216">  name       varchar(200) NOT NULL DEFAULT &#39;&#39;,
</li><li class="L217" rel="L217">  original   varchar(200) NOT NULL DEFAULT &#39;&#39;,
</li><li class="L218" rel="L218">  website    varchar(250) NOT NULL DEFAULT &#39;&#39;,
</li><li class="L219" rel="L219">  lang       language NOT NULL DEFAULT &#39;ja&#39;,
</li><li class="L220" rel="L220">  &#34;desc&#34;     text NOT NULL DEFAULT &#39;&#39;,
</li><li class="L221" rel="L221">  alias      varchar(500) NOT NULL DEFAULT &#39;&#39;,
</li><li class="L222" rel="L222">  l_wp       varchar(150)
</li><li class="L223" rel="L223">);
</li><li class="L224" rel="L224">
</li><li class="L225" rel="L225">-- producers_relations
</li><li class="L226" rel="L226">CREATE TABLE producers_relations (
</li><li class="L227" rel="L227">  id         integer NOT NULL,
</li><li class="L228" rel="L228">  pid        integer NOT NULL, -- producers.id
</li><li class="L229" rel="L229">  relation   producer_relation NOT NULL,
</li><li class="L230" rel="L230">  PRIMARY KEY(id, pid)
</li><li class="L231" rel="L231">);
</li><li class="L232" rel="L232">
</li><li class="L233" rel="L233">-- producers_relations_hist
</li><li class="L234" rel="L234">CREATE TABLE producers_relations_hist (
</li><li class="L235" rel="L235">  chid       integer NOT NULL,
</li><li class="L236" rel="L236">  pid        integer NOT NULL, -- producers.id
</li><li class="L237" rel="L237">  relation   producer_relation NOT NULL,
</li><li class="L238" rel="L238">  PRIMARY KEY(chid, pid)
</li><li class="L239" rel="L239">);
</li><li class="L240" rel="L240">
</li><li class="L241" rel="L241">-- quotes
</li><li class="L242" rel="L242">CREATE TABLE quotes (
</li><li class="L243" rel="L243">  vid integer NOT NULL,
</li><li class="L244" rel="L244">  quote varchar(250) NOT NULL,
</li><li class="L245" rel="L245">  PRIMARY KEY(vid, quote)
</li><li class="L246" rel="L246">);
</li><li class="L247" rel="L247">
</li><li class="L248" rel="L248">-- releases
</li><li class="L249" rel="L249">CREATE TABLE releases ( -- dbentry_type=r
</li><li class="L250" rel="L250">  id         SERIAL PRIMARY KEY,
</li><li class="L251" rel="L251">  locked     boolean NOT NULL DEFAULT FALSE,
</li><li class="L252" rel="L252">  hidden     boolean NOT NULL DEFAULT FALSE,
</li><li class="L253" rel="L253">  title      varchar(250) NOT NULL DEFAULT &#39;&#39;,
</li><li class="L254" rel="L254">  original   varchar(250) NOT NULL DEFAULT &#39;&#39;,
</li><li class="L255" rel="L255">  type       release_type NOT NULL DEFAULT &#39;complete&#39;,
</li><li class="L256" rel="L256">  website    varchar(250) NOT NULL DEFAULT &#39;&#39;,
</li><li class="L257" rel="L257">  catalog    varchar(50) NOT NULL DEFAULT &#39;&#39;,
</li><li class="L258" rel="L258">  gtin       bigint NOT NULL DEFAULT 0,
</li><li class="L259" rel="L259">  released   integer NOT NULL DEFAULT 0,
</li><li class="L260" rel="L260">  notes      text NOT NULL DEFAULT &#39;&#39;,
</li><li class="L261" rel="L261">  minage     smallint,
</li><li class="L262" rel="L262">  patch      boolean NOT NULL DEFAULT FALSE,
</li><li class="L263" rel="L263">  freeware   boolean NOT NULL DEFAULT FALSE,
</li><li class="L264" rel="L264">  doujin     boolean NOT NULL DEFAULT FALSE,
</li><li class="L265" rel="L265">  resolution smallint NOT NULL DEFAULT 0,
</li><li class="L266" rel="L266">  voiced     smallint NOT NULL DEFAULT 0,
</li><li class="L267" rel="L267">  ani_story  smallint NOT NULL DEFAULT 0,
</li><li class="L268" rel="L268">  ani_ero    smallint NOT NULL DEFAULT 0,
</li><li class="L269" rel="L269">  uncensored boolean NOT NULL DEFAULT FALSE
</li><li class="L270" rel="L270">);
</li><li class="L271" rel="L271">
</li><li class="L272" rel="L272">-- releases_hist
</li><li class="L273" rel="L273">CREATE TABLE releases_hist (
</li><li class="L274" rel="L274">  chid       integer NOT NULL PRIMARY KEY,
</li><li class="L275" rel="L275">  title      varchar(250) NOT NULL DEFAULT &#39;&#39;,
</li><li class="L276" rel="L276">  original   varchar(250) NOT NULL DEFAULT &#39;&#39;,
</li><li class="L277" rel="L277">  type       release_type NOT NULL DEFAULT &#39;complete&#39;,
</li><li class="L278" rel="L278">  website    varchar(250) NOT NULL DEFAULT &#39;&#39;,
</li><li class="L279" rel="L279">  catalog    varchar(50) NOT NULL DEFAULT &#39;&#39;,
</li><li class="L280" rel="L280">  gtin       bigint NOT NULL DEFAULT 0,
</li><li class="L281" rel="L281">  released   integer NOT NULL DEFAULT 0,
</li><li class="L282" rel="L282">  notes      text NOT NULL DEFAULT &#39;&#39;,
</li><li class="L283" rel="L283">  minage     smallint,
</li><li class="L284" rel="L284">  patch      boolean NOT NULL DEFAULT FALSE,
</li><li class="L285" rel="L285">  freeware   boolean NOT NULL DEFAULT FALSE,
</li><li class="L286" rel="L286">  doujin     boolean NOT NULL DEFAULT FALSE,
</li><li class="L287" rel="L287">  resolution smallint NOT NULL DEFAULT 0,
</li><li class="L288" rel="L288">  voiced     smallint NOT NULL DEFAULT 0,
</li><li class="L289" rel="L289">  ani_story  smallint NOT NULL DEFAULT 0,
</li><li class="L290" rel="L290">  ani_ero    smallint NOT NULL DEFAULT 0,
</li><li class="L291" rel="L291">  uncensored boolean NOT NULL DEFAULT FALSE
</li><li class="L292" rel="L292">);
</li><li class="L293" rel="L293">
</li><li class="L294" rel="L294">-- releases_lang
</li><li class="L295" rel="L295">CREATE TABLE releases_lang (
</li><li class="L296" rel="L296">  id         integer NOT NULL,
</li><li class="L297" rel="L297">  lang       language NOT NULL,
</li><li class="L298" rel="L298">  PRIMARY KEY(id, lang)
</li><li class="L299" rel="L299">);
</li><li class="L300" rel="L300">
</li><li class="L301" rel="L301">-- releases_lang_hist
</li><li class="L302" rel="L302">CREATE TABLE releases_lang_hist (
</li><li class="L303" rel="L303">  chid       integer NOT NULL,
</li><li class="L304" rel="L304">  lang       language NOT NULL,
</li><li class="L305" rel="L305">  PRIMARY KEY(chid, lang)
</li><li class="L306" rel="L306">);
</li><li class="L307" rel="L307">
</li><li class="L308" rel="L308">-- releases_media
</li><li class="L309" rel="L309">CREATE TABLE releases_media (
</li><li class="L310" rel="L310">  id         integer NOT NULL,
</li><li class="L311" rel="L311">  medium     medium NOT NULL,
</li><li class="L312" rel="L312">  qty        smallint NOT NULL DEFAULT 1,
</li><li class="L313" rel="L313">  PRIMARY KEY(id, medium, qty)
</li><li class="L314" rel="L314">);
</li><li class="L315" rel="L315">
</li><li class="L316" rel="L316">-- releases_media_hist
</li><li class="L317" rel="L317">CREATE TABLE releases_media_hist (
</li><li class="L318" rel="L318">  chid       integer NOT NULL,
</li><li class="L319" rel="L319">  medium     medium NOT NULL,
</li><li class="L320" rel="L320">  qty        smallint NOT NULL DEFAULT 1,
</li><li class="L321" rel="L321">  PRIMARY KEY(chid, medium, qty)
</li><li class="L322" rel="L322">);
</li><li class="L323" rel="L323">
</li><li class="L324" rel="L324">-- releases_platforms
</li><li class="L325" rel="L325">CREATE TABLE releases_platforms (
</li><li class="L326" rel="L326">  id         integer NOT NULL,
</li><li class="L327" rel="L327">  platform   platform NOT NULL,
</li><li class="L328" rel="L328">  PRIMARY KEY(id, platform)
</li><li class="L329" rel="L329">);
</li><li class="L330" rel="L330">
</li><li class="L331" rel="L331">-- releases_platforms_hist
</li><li class="L332" rel="L332">CREATE TABLE releases_platforms_hist (
</li><li class="L333" rel="L333">  chid       integer NOT NULL,
</li><li class="L334" rel="L334">  platform   platform NOT NULL,
</li><li class="L335" rel="L335">  PRIMARY KEY(chid, platform)
</li><li class="L336" rel="L336">);
</li><li class="L337" rel="L337">
</li><li class="L338" rel="L338">-- releases_producers
</li><li class="L339" rel="L339">CREATE TABLE releases_producers (
</li><li class="L340" rel="L340">  id         integer NOT NULL,
</li><li class="L341" rel="L341">  pid        integer NOT NULL, -- producers.id
</li><li class="L342" rel="L342">  developer  boolean NOT NULL DEFAULT FALSE,
</li><li class="L343" rel="L343">  publisher  boolean NOT NULL DEFAULT TRUE,
</li><li class="L344" rel="L344">  CHECK(developer OR publisher),
</li><li class="L345" rel="L345">  PRIMARY KEY(id, pid)
</li><li class="L346" rel="L346">);
</li><li class="L347" rel="L347">
</li><li class="L348" rel="L348">-- releases_producers_hist
</li><li class="L349" rel="L349">CREATE TABLE releases_producers_hist (
</li><li class="L350" rel="L350">  chid       integer NOT NULL,
</li><li class="L351" rel="L351">  pid        integer NOT NULL, -- producers.id
</li><li class="L352" rel="L352">  developer  boolean NOT NULL DEFAULT FALSE,
</li><li class="L353" rel="L353">  publisher  boolean NOT NULL DEFAULT TRUE,
</li><li class="L354" rel="L354">  CHECK(developer OR publisher),
</li><li class="L355" rel="L355">  PRIMARY KEY(chid, pid)
</li><li class="L356" rel="L356">);
</li><li class="L357" rel="L357">
</li><li class="L358" rel="L358">-- releases_vn
</li><li class="L359" rel="L359">CREATE TABLE releases_vn (
</li><li class="L360" rel="L360">  id         integer NOT NULL,
</li><li class="L361" rel="L361">  vid        integer NOT NULL, -- vn.id
</li><li class="L362" rel="L362">  PRIMARY KEY(id, vid)
</li><li class="L363" rel="L363">);
</li><li class="L364" rel="L364">
</li><li class="L365" rel="L365">-- releases_vn_hist
</li><li class="L366" rel="L366">CREATE TABLE releases_vn_hist (
</li><li class="L367" rel="L367">  chid       integer NOT NULL,
</li><li class="L368" rel="L368">  vid        integer NOT NULL, -- vn.id
</li><li class="L369" rel="L369">  PRIMARY KEY(chid, vid)
</li><li class="L370" rel="L370">);
</li><li class="L371" rel="L371">
</li><li class="L372" rel="L372">-- relgraphs
</li><li class="L373" rel="L373">CREATE TABLE relgraphs (
</li><li class="L374" rel="L374">  id SERIAL PRIMARY KEY,
</li><li class="L375" rel="L375">  svg xml NOT NULL
</li><li class="L376" rel="L376">);
</li><li class="L377" rel="L377">
</li><li class="L378" rel="L378">-- rlists
</li><li class="L379" rel="L379">CREATE TABLE rlists (
</li><li class="L380" rel="L380">  uid integer NOT NULL DEFAULT 0,
</li><li class="L381" rel="L381">  rid integer NOT NULL DEFAULT 0,
</li><li class="L382" rel="L382">  status smallint NOT NULL DEFAULT 0,
</li><li class="L383" rel="L383">  added timestamptz NOT NULL DEFAULT NOW(),
</li><li class="L384" rel="L384">  PRIMARY KEY(uid, rid)
</li><li class="L385" rel="L385">);
</li><li class="L386" rel="L386">
</li><li class="L387" rel="L387">-- screenshots
</li><li class="L388" rel="L388">CREATE TABLE screenshots (
</li><li class="L389" rel="L389">  id SERIAL NOT NULL PRIMARY KEY,
</li><li class="L390" rel="L390">  width smallint NOT NULL DEFAULT 0,
</li><li class="L391" rel="L391">  height smallint NOT NULL DEFAULT 0
</li><li class="L392" rel="L392">);
</li><li class="L393" rel="L393">
</li><li class="L394" rel="L394">-- sessions
</li><li class="L395" rel="L395">CREATE TABLE sessions (
</li><li class="L396" rel="L396">  uid integer NOT NULL,
</li><li class="L397" rel="L397">  token bytea NOT NULL,
</li><li class="L398" rel="L398">  added timestamptz NOT NULL DEFAULT NOW(),
</li><li class="L399" rel="L399">  lastused timestamptz NOT NULL DEFAULT NOW(),
</li><li class="L400" rel="L400">  PRIMARY KEY (uid, token)
</li><li class="L401" rel="L401">);
</li><li class="L402" rel="L402">
</li><li class="L403" rel="L403">-- staff
</li><li class="L404" rel="L404">CREATE TABLE staff ( -- dbentry_type=s
</li><li class="L405" rel="L405">  id         SERIAL PRIMARY KEY,
</li><li class="L406" rel="L406">  locked     boolean NOT NULL DEFAULT FALSE,
</li><li class="L407" rel="L407">  hidden     boolean NOT NULL DEFAULT FALSE,
</li><li class="L408" rel="L408">  aid        integer NOT NULL DEFAULT 0, -- staff_alias.aid
</li><li class="L409" rel="L409">  gender     gender NOT NULL DEFAULT &#39;unknown&#39;,
</li><li class="L410" rel="L410">  lang       language NOT NULL DEFAULT &#39;ja&#39;,
</li><li class="L411" rel="L411">  &#34;desc&#34;     text NOT NULL DEFAULT &#39;&#39;,
</li><li class="L412" rel="L412">  l_wp       varchar(150) NOT NULL DEFAULT &#39;&#39;,
</li><li class="L413" rel="L413">  l_site     varchar(250) NOT NULL DEFAULT &#39;&#39;,
</li><li class="L414" rel="L414">  l_twitter  varchar(16) NOT NULL DEFAULT &#39;&#39;,
</li><li class="L415" rel="L415">  l_anidb    integer
</li><li class="L416" rel="L416">);
</li><li class="L417" rel="L417">
</li><li class="L418" rel="L418">-- staff_hist
</li><li class="L419" rel="L419">CREATE TABLE staff_hist (
</li><li class="L420" rel="L420">  chid       integer NOT NULL PRIMARY KEY,
</li><li class="L421" rel="L421">  aid        integer NOT NULL DEFAULT 0, -- Can&#39;t refer to staff_alias.id, because the alias might have been deleted
</li><li class="L422" rel="L422">  gender     gender NOT NULL DEFAULT &#39;unknown&#39;,
</li><li class="L423" rel="L423">  lang       language NOT NULL DEFAULT &#39;ja&#39;,
</li><li class="L424" rel="L424">  &#34;desc&#34;     text NOT NULL DEFAULT &#39;&#39;,
</li><li class="L425" rel="L425">  l_wp       varchar(150) NOT NULL DEFAULT &#39;&#39;,
</li><li class="L426" rel="L426">  l_site     varchar(250) NOT NULL DEFAULT &#39;&#39;,
</li><li class="L427" rel="L427">  l_twitter  varchar(16) NOT NULL DEFAULT &#39;&#39;,
</li><li class="L428" rel="L428">  l_anidb    integer
</li><li class="L429" rel="L429">);
</li><li class="L430" rel="L430">
</li><li class="L431" rel="L431">-- staff_alias
</li><li class="L432" rel="L432">CREATE TABLE staff_alias (
</li><li class="L433" rel="L433">  id         integer NOT NULL,
</li><li class="L434" rel="L434">  aid        SERIAL PRIMARY KEY, -- Globally unique ID of this alias
</li><li class="L435" rel="L435">  name       varchar(200) NOT NULL DEFAULT &#39;&#39;,
</li><li class="L436" rel="L436">  original   varchar(200) NOT NULL DEFAULT &#39;&#39;
</li><li class="L437" rel="L437">);
</li><li class="L438" rel="L438">
</li><li class="L439" rel="L439">-- staff_alias_hist
</li><li class="L440" rel="L440">CREATE TABLE staff_alias_hist (
</li><li class="L441" rel="L441">  chid       integer NOT NULL,
</li><li class="L442" rel="L442">  aid        integer NOT NULL, -- staff_alias.aid, but can&#39;t reference it because the alias may have been deleted
</li><li class="L443" rel="L443">  name       varchar(200) NOT NULL DEFAULT &#39;&#39;,
</li><li class="L444" rel="L444">  original   varchar(200) NOT NULL DEFAULT &#39;&#39;,
</li><li class="L445" rel="L445">  PRIMARY KEY(chid, aid)
</li><li class="L446" rel="L446">);
</li><li class="L447" rel="L447">
</li><li class="L448" rel="L448">-- stats_cache
</li><li class="L449" rel="L449">CREATE TABLE stats_cache (
</li><li class="L450" rel="L450">  section varchar(25) NOT NULL PRIMARY KEY,
</li><li class="L451" rel="L451">  count integer NOT NULL DEFAULT 0
</li><li class="L452" rel="L452">);
</li><li class="L453" rel="L453">
</li><li class="L454" rel="L454">-- tags
</li><li class="L455" rel="L455">CREATE TABLE tags (
</li><li class="L456" rel="L456">  id SERIAL NOT NULL PRIMARY KEY,
</li><li class="L457" rel="L457">  name varchar(250) NOT NULL UNIQUE,
</li><li class="L458" rel="L458">  description text NOT NULL DEFAULT &#39;&#39;,
</li><li class="L459" rel="L459">  meta boolean NOT NULL DEFAULT FALSE,
</li><li class="L460" rel="L460">  added timestamptz NOT NULL DEFAULT NOW(),
</li><li class="L461" rel="L461">  state smallint NOT NULL DEFAULT 0,
</li><li class="L462" rel="L462">  c_items integer NOT NULL DEFAULT 0,
</li><li class="L463" rel="L463">  addedby integer NOT NULL DEFAULT 0,
</li><li class="L464" rel="L464">  cat tag_category NOT NULL DEFAULT &#39;cont&#39;
</li><li class="L465" rel="L465">);
</li><li class="L466" rel="L466">
</li><li class="L467" rel="L467">-- tags_aliases
</li><li class="L468" rel="L468">CREATE TABLE tags_aliases (
</li><li class="L469" rel="L469">  alias varchar(250) NOT NULL PRIMARY KEY,
</li><li class="L470" rel="L470">  tag integer NOT NULL
</li><li class="L471" rel="L471">);
</li><li class="L472" rel="L472">
</li><li class="L473" rel="L473">-- tags_parents
</li><li class="L474" rel="L474">CREATE TABLE tags_parents (
</li><li class="L475" rel="L475">  tag integer NOT NULL,
</li><li class="L476" rel="L476">  parent integer NOT NULL,
</li><li class="L477" rel="L477">  PRIMARY KEY(tag, parent)
</li><li class="L478" rel="L478">);
</li><li class="L479" rel="L479">
</li><li class="L480" rel="L480">-- tags_vn
</li><li class="L481" rel="L481">CREATE TABLE tags_vn (
</li><li class="L482" rel="L482">  tag integer NOT NULL,
</li><li class="L483" rel="L483">  vid integer NOT NULL,
</li><li class="L484" rel="L484">  uid integer NOT NULL,
</li><li class="L485" rel="L485">  vote smallint NOT NULL DEFAULT 3 CHECK (vote &gt;= -3 AND vote &lt;= 3 AND vote &lt;&gt; 0),
</li><li class="L486" rel="L486">  spoiler smallint CHECK(spoiler &gt;= 0 AND spoiler &lt;= 2),
</li><li class="L487" rel="L487">  date timestamptz NOT NULL DEFAULT NOW(),
</li><li class="L488" rel="L488">  ignore boolean NOT NULL DEFAULT false,
</li><li class="L489" rel="L489">  PRIMARY KEY(tag, vid, uid)
</li><li class="L490" rel="L490">);
</li><li class="L491" rel="L491">
</li><li class="L492" rel="L492">-- tags_vn_inherit
</li><li class="L493" rel="L493">CREATE TABLE tags_vn_inherit (
</li><li class="L494" rel="L494">  tag integer NOT NULL,
</li><li class="L495" rel="L495">  vid integer NOT NULL,
</li><li class="L496" rel="L496">  users integer NOT NULL,
</li><li class="L497" rel="L497">  rating real NOT NULL,
</li><li class="L498" rel="L498">  spoiler smallint NOT NULL
</li><li class="L499" rel="L499">);
</li><li class="L500" rel="L500">
</li><li class="L501" rel="L501">-- threads
</li><li class="L502" rel="L502">CREATE TABLE threads (
</li><li class="L503" rel="L503">  id SERIAL NOT NULL PRIMARY KEY,
</li><li class="L504" rel="L504">  title varchar(50) NOT NULL DEFAULT &#39;&#39;,
</li><li class="L505" rel="L505">  locked boolean NOT NULL DEFAULT FALSE,
</li><li class="L506" rel="L506">  hidden boolean NOT NULL DEFAULT FALSE,
</li><li class="L507" rel="L507">  count smallint NOT NULL DEFAULT 0,
</li><li class="L508" rel="L508">  poll_question varchar(100),
</li><li class="L509" rel="L509">  poll_max_options smallint NOT NULL DEFAULT 1,
</li><li class="L510" rel="L510">  poll_preview boolean NOT NULL DEFAULT FALSE,
</li><li class="L511" rel="L511">  poll_recast boolean NOT NULL DEFAULT FALSE
</li><li class="L512" rel="L512">);
</li><li class="L513" rel="L513">
</li><li class="L514" rel="L514">-- threads_poll_options
</li><li class="L515" rel="L515">CREATE TABLE threads_poll_options (
</li><li class="L516" rel="L516">  id     SERIAL PRIMARY KEY,
</li><li class="L517" rel="L517">  tid    integer NOT NULL,
</li><li class="L518" rel="L518">  option varchar(100) NOT NULL
</li><li class="L519" rel="L519">);
</li><li class="L520" rel="L520">
</li><li class="L521" rel="L521">-- threads_poll_votes
</li><li class="L522" rel="L522">CREATE TABLE threads_poll_votes (
</li><li class="L523" rel="L523">  tid   integer NOT NULL,
</li><li class="L524" rel="L524">  uid   integer NOT NULL,
</li><li class="L525" rel="L525">  optid integer NOT NULL,
</li><li class="L526" rel="L526">  PRIMARY KEY (tid, uid, optid)
</li><li class="L527" rel="L527">);
</li><li class="L528" rel="L528">
</li><li class="L529" rel="L529">-- threads_posts
</li><li class="L530" rel="L530">CREATE TABLE threads_posts (
</li><li class="L531" rel="L531">  tid integer NOT NULL DEFAULT 0,
</li><li class="L532" rel="L532">  num smallint NOT NULL DEFAULT 0,
</li><li class="L533" rel="L533">  uid integer NOT NULL DEFAULT 0,
</li><li class="L534" rel="L534">  date timestamptz NOT NULL DEFAULT NOW(),
</li><li class="L535" rel="L535">  edited timestamptz,
</li><li class="L536" rel="L536">  msg text NOT NULL DEFAULT &#39;&#39;,
</li><li class="L537" rel="L537">  hidden boolean NOT NULL DEFAULT FALSE,
</li><li class="L538" rel="L538">  PRIMARY KEY(tid, num)
</li><li class="L539" rel="L539">);
</li><li class="L540" rel="L540">
</li><li class="L541" rel="L541">-- threads_boards
</li><li class="L542" rel="L542">CREATE TABLE threads_boards (
</li><li class="L543" rel="L543">  tid integer NOT NULL DEFAULT 0,
</li><li class="L544" rel="L544">  type board_type NOT NULL,
</li><li class="L545" rel="L545">  iid integer NOT NULL DEFAULT 0,
</li><li class="L546" rel="L546">  PRIMARY KEY(tid, type, iid)
</li><li class="L547" rel="L547">);
</li><li class="L548" rel="L548">
</li><li class="L549" rel="L549">-- traits
</li><li class="L550" rel="L550">CREATE TABLE traits (
</li><li class="L551" rel="L551">  id SERIAL PRIMARY KEY,
</li><li class="L552" rel="L552">  name varchar(250) NOT NULL,
</li><li class="L553" rel="L553">  alias varchar(500) NOT NULL DEFAULT &#39;&#39;,
</li><li class="L554" rel="L554">  description text NOT NULL DEFAULT &#39;&#39;,
</li><li class="L555" rel="L555">  meta boolean NOT NULL DEFAULT false,
</li><li class="L556" rel="L556">  added timestamptz NOT NULL DEFAULT NOW(),
</li><li class="L557" rel="L557">  state smallint NOT NULL DEFAULT 0,
</li><li class="L558" rel="L558">  addedby integer NOT NULL DEFAULT 0,
</li><li class="L559" rel="L559">  &#34;group&#34; integer,
</li><li class="L560" rel="L560">  &#34;order&#34; smallint NOT NULL DEFAULT 0,
</li><li class="L561" rel="L561">  sexual boolean NOT NULL DEFAULT false,
</li><li class="L562" rel="L562">  c_items integer NOT NULL DEFAULT 0
</li><li class="L563" rel="L563">);
</li><li class="L564" rel="L564">
</li><li class="L565" rel="L565">-- traits_chars
</li><li class="L566" rel="L566">-- This table is a cache for the data in chars_traits and includes child traits
</li><li class="L567" rel="L567">-- into parent traits. In order to improve performance, there are no foreign
</li><li class="L568" rel="L568">-- key constraints on this table.
</li><li class="L569" rel="L569">CREATE TABLE traits_chars (
</li><li class="L570" rel="L570">  cid integer NOT NULL,  -- chars (id)
</li><li class="L571" rel="L571">  tid integer NOT NULL,  -- traits (id)
</li><li class="L572" rel="L572">  spoil smallint NOT NULL DEFAULT 0,
</li><li class="L573" rel="L573">  PRIMARY KEY(cid, tid)
</li><li class="L574" rel="L574">);
</li><li class="L575" rel="L575">
</li><li class="L576" rel="L576">-- traits_parents
</li><li class="L577" rel="L577">CREATE TABLE traits_parents (
</li><li class="L578" rel="L578">  trait integer NOT NULL,
</li><li class="L579" rel="L579">  parent integer NOT NULL,
</li><li class="L580" rel="L580">  PRIMARY KEY(trait, parent)
</li><li class="L581" rel="L581">);
</li><li class="L582" rel="L582">
</li><li class="L583" rel="L583">-- users
</li><li class="L584" rel="L584">CREATE TABLE users (
</li><li class="L585" rel="L585">  id SERIAL NOT NULL PRIMARY KEY,
</li><li class="L586" rel="L586">  username varchar(20) NOT NULL UNIQUE,
</li><li class="L587" rel="L587">  mail varchar(100) NOT NULL,
</li><li class="L588" rel="L588">  perm smallint NOT NULL DEFAULT 1+4+16,
</li><li class="L589" rel="L589">  -- Interpretation of the passwd column depends on its length:
</li><li class="L590" rel="L590">  -- * 20 bytes: Password reset token (sha1(lower_hex(20 bytes of random data)))
</li><li class="L591" rel="L591">  -- * 46 bytes: scrypt password
</li><li class="L592" rel="L592">  --   4 bytes: N (big endian)
</li><li class="L593" rel="L593">  --   1 byte: r
</li><li class="L594" rel="L594">  --   1 byte: p
</li><li class="L595" rel="L595">  --   8 bytes: salt
</li><li class="L596" rel="L596">  --   32 bytes: scrypt(passwd, global_salt + salt, N, r, p, 32)
</li><li class="L597" rel="L597">  -- * Anything else: Invalid, account disabled.
</li><li class="L598" rel="L598">  passwd bytea NOT NULL DEFAULT &#39;&#39;,
</li><li class="L599" rel="L599">  registered timestamptz NOT NULL DEFAULT NOW(),
</li><li class="L600" rel="L600">  c_votes integer NOT NULL DEFAULT 0,
</li><li class="L601" rel="L601">  c_changes integer NOT NULL DEFAULT 0,
</li><li class="L602" rel="L602">  ip inet NOT NULL DEFAULT &#39;0.0.0.0&#39;,
</li><li class="L603" rel="L603">  c_tags integer NOT NULL DEFAULT 0,
</li><li class="L604" rel="L604">  ign_votes boolean NOT NULL DEFAULT FALSE,
</li><li class="L605" rel="L605">  email_confirmed boolean NOT NULL DEFAULT FALSE
</li><li class="L606" rel="L606">);
</li><li class="L607" rel="L607">
</li><li class="L608" rel="L608">-- users_prefs
</li><li class="L609" rel="L609">CREATE TABLE users_prefs (
</li><li class="L610" rel="L610">  uid integer NOT NULL,
</li><li class="L611" rel="L611">  key prefs_key NOT NULL,
</li><li class="L612" rel="L612">  value varchar NOT NULL,
</li><li class="L613" rel="L613">  PRIMARY KEY(uid, key)
</li><li class="L614" rel="L614">);
</li><li class="L615" rel="L615">
</li><li class="L616" rel="L616">-- vn
</li><li class="L617" rel="L617">CREATE TABLE vn ( -- dbentry_type=v
</li><li class="L618" rel="L618">  id         SERIAL PRIMARY KEY,
</li><li class="L619" rel="L619">  locked     boolean NOT NULL DEFAULT FALSE,
</li><li class="L620" rel="L620">  hidden     boolean NOT NULL DEFAULT FALSE,
</li><li class="L621" rel="L621">  title      varchar(250) NOT NULL DEFAULT &#39;&#39;,
</li><li class="L622" rel="L622">  original   varchar(250) NOT NULL DEFAULT &#39;&#39;,
</li><li class="L623" rel="L623">  alias      varchar(500) NOT NULL DEFAULT &#39;&#39;,
</li><li class="L624" rel="L624">  length     smallint NOT NULL DEFAULT 0,
</li><li class="L625" rel="L625">  img_nsfw   boolean NOT NULL DEFAULT FALSE,
</li><li class="L626" rel="L626">  image      integer NOT NULL DEFAULT 0,
</li><li class="L627" rel="L627">  &#34;desc&#34;     text NOT NULL DEFAULT &#39;&#39;,
</li><li class="L628" rel="L628">  l_wp       varchar(150) NOT NULL DEFAULT &#39;&#39;,
</li><li class="L629" rel="L629">  l_encubed  varchar(100) NOT NULL DEFAULT &#39;&#39;,
</li><li class="L630" rel="L630">  l_renai    varchar(100) NOT NULL DEFAULT &#39;&#39;,
</li><li class="L631" rel="L631">  rgraph     integer, -- relgraphs.id
</li><li class="L632" rel="L632">  c_released integer NOT NULL DEFAULT 0,
</li><li class="L633" rel="L633">  c_languages language[] NOT NULL DEFAULT &#39;{}&#39;,
</li><li class="L634" rel="L634">  c_olang    language[] NOT NULL DEFAULT &#39;{}&#39;,
</li><li class="L635" rel="L635">  c_platforms platform[] NOT NULL DEFAULT &#39;{}&#39;,
</li><li class="L636" rel="L636">  c_popularity real,
</li><li class="L637" rel="L637">  c_rating   real,
</li><li class="L638" rel="L638">  c_votecount integer NOT NULL DEFAULT 0,
</li><li class="L639" rel="L639">  c_search   text
</li><li class="L640" rel="L640">);
</li><li class="L641" rel="L641">
</li><li class="L642" rel="L642">-- vn_hist
</li><li class="L643" rel="L643">CREATE TABLE vn_hist (
</li><li class="L644" rel="L644">  chid       integer NOT NULL PRIMARY KEY,
</li><li class="L645" rel="L645">  title      varchar(250) NOT NULL DEFAULT &#39;&#39;,
</li><li class="L646" rel="L646">  original   varchar(250) NOT NULL DEFAULT &#39;&#39;,
</li><li class="L647" rel="L647">  alias      varchar(500) NOT NULL DEFAULT &#39;&#39;,
</li><li class="L648" rel="L648">  length     smallint NOT NULL DEFAULT 0,
</li><li class="L649" rel="L649">  img_nsfw   boolean NOT NULL DEFAULT FALSE,
</li><li class="L650" rel="L650">  image      integer NOT NULL DEFAULT 0,
</li><li class="L651" rel="L651">  &#34;desc&#34;     text NOT NULL DEFAULT &#39;&#39;,
</li><li class="L652" rel="L652">  l_wp       varchar(150) NOT NULL DEFAULT &#39;&#39;,
</li><li class="L653" rel="L653">  l_encubed  varchar(100) NOT NULL DEFAULT &#39;&#39;,
</li><li class="L654" rel="L654">  l_renai    varchar(100) NOT NULL DEFAULT &#39;&#39;
</li><li class="L655" rel="L655">);
</li><li class="L656" rel="L656">
</li><li class="L657" rel="L657">-- vn_anime
</li><li class="L658" rel="L658">CREATE TABLE vn_anime (
</li><li class="L659" rel="L659">  id         integer NOT NULL,
</li><li class="L660" rel="L660">  aid        integer NOT NULL, -- anime.id
</li><li class="L661" rel="L661">  PRIMARY KEY(id, aid)
</li><li class="L662" rel="L662">);
</li><li class="L663" rel="L663">
</li><li class="L664" rel="L664">-- vn_anime_hist
</li><li class="L665" rel="L665">CREATE TABLE vn_anime_hist (
</li><li class="L666" rel="L666">  chid       integer NOT NULL,
</li><li class="L667" rel="L667">  aid        integer NOT NULL, -- anime.id
</li><li class="L668" rel="L668">  PRIMARY KEY(chid, aid)
</li><li class="L669" rel="L669">);
</li><li class="L670" rel="L670">
</li><li class="L671" rel="L671">-- vn_relations
</li><li class="L672" rel="L672">CREATE TABLE vn_relations (
</li><li class="L673" rel="L673">  id         integer NOT NULL,
</li><li class="L674" rel="L674">  vid        integer NOT NULL, -- vn.id
</li><li class="L675" rel="L675">  relation   vn_relation NOT NULL,
</li><li class="L676" rel="L676">  official   boolean NOT NULL DEFAULT TRUE,
</li><li class="L677" rel="L677">  PRIMARY KEY(id, vid)
</li><li class="L678" rel="L678">);
</li><li class="L679" rel="L679">
</li><li class="L680" rel="L680">-- vn_relations_hist
</li><li class="L681" rel="L681">CREATE TABLE vn_relations_hist (
</li><li class="L682" rel="L682">  chid       integer NOT NULL,
</li><li class="L683" rel="L683">  vid        integer NOT NULL, -- vn.id
</li><li class="L684" rel="L684">  relation   vn_relation NOT NULL,
</li><li class="L685" rel="L685">  official   boolean NOT NULL DEFAULT TRUE,
</li><li class="L686" rel="L686">  PRIMARY KEY(chid, vid)
</li><li class="L687" rel="L687">);
</li><li class="L688" rel="L688">
</li><li class="L689" rel="L689">-- vn_screenshots
</li><li class="L690" rel="L690">CREATE TABLE vn_screenshots (
</li><li class="L691" rel="L691">  id         integer NOT NULL,
</li><li class="L692" rel="L692">  scr        integer NOT NULL, -- screenshots.id
</li><li class="L693" rel="L693">  rid        integer,          -- releases.id (only NULL for old revisions, nowadays not allowed anymore)
</li><li class="L694" rel="L694">  nsfw       boolean NOT NULL DEFAULT FALSE,
</li><li class="L695" rel="L695">  PRIMARY KEY(id, scr)
</li><li class="L696" rel="L696">);
</li><li class="L697" rel="L697">
</li><li class="L698" rel="L698">-- vn_screenshots_hist
</li><li class="L699" rel="L699">CREATE TABLE vn_screenshots_hist (
</li><li class="L700" rel="L700">  chid       integer NOT NULL,
</li><li class="L701" rel="L701">  scr        integer NOT NULL,
</li><li class="L702" rel="L702">  rid        integer,
</li><li class="L703" rel="L703">  nsfw       boolean NOT NULL DEFAULT FALSE,
</li><li class="L704" rel="L704">  PRIMARY KEY(chid, scr)
</li><li class="L705" rel="L705">);
</li><li class="L706" rel="L706">
</li><li class="L707" rel="L707">-- vn_seiyuu
</li><li class="L708" rel="L708">CREATE TABLE vn_seiyuu (
</li><li class="L709" rel="L709">  id         integer NOT NULL,
</li><li class="L710" rel="L710">  aid        integer NOT NULL, -- staff_alias.aid
</li><li class="L711" rel="L711">  cid        integer NOT NULL, -- chars.id
</li><li class="L712" rel="L712">  note       varchar(250) NOT NULL DEFAULT &#39;&#39;,
</li><li class="L713" rel="L713">  PRIMARY KEY (id, aid, cid)
</li><li class="L714" rel="L714">);
</li><li class="L715" rel="L715">
</li><li class="L716" rel="L716">-- vn_seiyuu_hist
</li><li class="L717" rel="L717">CREATE TABLE vn_seiyuu_hist (
</li><li class="L718" rel="L718">  chid       integer NOT NULL,
</li><li class="L719" rel="L719">  aid        integer NOT NULL, -- staff_alias.aid, but can&#39;t reference it because the alias may have been deleted
</li><li class="L720" rel="L720">  cid        integer NOT NULL, -- chars.id
</li><li class="L721" rel="L721">  note       varchar(250) NOT NULL DEFAULT &#39;&#39;,
</li><li class="L722" rel="L722">  PRIMARY KEY (chid, aid, cid)
</li><li class="L723" rel="L723">);
</li><li class="L724" rel="L724">
</li><li class="L725" rel="L725">-- vn_staff
</li><li class="L726" rel="L726">CREATE TABLE vn_staff (
</li><li class="L727" rel="L727">  id         integer NOT NULL,
</li><li class="L728" rel="L728">  aid        integer NOT NULL, -- staff_alias.aid
</li><li class="L729" rel="L729">  role       credit_type NOT NULL DEFAULT &#39;staff&#39;,
</li><li class="L730" rel="L730">  note       varchar(250) NOT NULL DEFAULT &#39;&#39;,
</li><li class="L731" rel="L731">  PRIMARY KEY (id, aid, role)
</li><li class="L732" rel="L732">);
</li><li class="L733" rel="L733">
</li><li class="L734" rel="L734">-- vn_staff_hist
</li><li class="L735" rel="L735">CREATE TABLE vn_staff_hist (
</li><li class="L736" rel="L736">  chid       integer NOT NULL,
</li><li class="L737" rel="L737">  aid        integer NOT NULL, -- See note at vn_seiyuu_hist.aid
</li><li class="L738" rel="L738">  role       credit_type NOT NULL DEFAULT &#39;staff&#39;,
</li><li class="L739" rel="L739">  note       varchar(250) NOT NULL DEFAULT &#39;&#39;,
</li><li class="L740" rel="L740">  PRIMARY KEY (chid, aid, role)
</li><li class="L741" rel="L741">);
</li><li class="L742" rel="L742">
</li><li class="L743" rel="L743">-- vnlists
</li><li class="L744" rel="L744">CREATE TABLE vnlists (
</li><li class="L745" rel="L745">  uid integer NOT NULL,
</li><li class="L746" rel="L746">  vid integer NOT NULL,
</li><li class="L747" rel="L747">  status smallint NOT NULL DEFAULT 0,
</li><li class="L748" rel="L748">  added TIMESTAMPTZ NOT NULL DEFAULT NOW(),
</li><li class="L749" rel="L749">  notes varchar NOT NULL DEFAULT &#39;&#39;,
</li><li class="L750" rel="L750">  PRIMARY KEY(uid, vid)
</li><li class="L751" rel="L751">);
</li><li class="L752" rel="L752">
</li><li class="L753" rel="L753">-- votes
</li><li class="L754" rel="L754">CREATE TABLE votes (
</li><li class="L755" rel="L755">  vid integer NOT NULL DEFAULT 0,
</li><li class="L756" rel="L756">  uid integer NOT NULL DEFAULT 0,
</li><li class="L757" rel="L757">  vote integer NOT NULL DEFAULT 0,
</li><li class="L758" rel="L758">  date timestamptz NOT NULL DEFAULT NOW(),
</li><li class="L759" rel="L759">  PRIMARY KEY(vid, uid)
</li><li class="L760" rel="L760">);
</li><li class="L761" rel="L761">
</li><li class="L762" rel="L762">-- wlists
</li><li class="L763" rel="L763">CREATE TABLE wlists (
</li><li class="L764" rel="L764">  uid integer NOT NULL DEFAULT 0,
</li><li class="L765" rel="L765">  vid integer NOT NULL DEFAULT 0,
</li><li class="L766" rel="L766">  wstat smallint NOT NULL DEFAULT 0,
</li><li class="L767" rel="L767">  added timestamptz NOT NULL DEFAULT NOW(),
</li><li class="L768" rel="L768">  PRIMARY KEY(uid, vid)
</li><li class="L769" rel="L769">);
</li><li class="L770" rel="L770"></li></ol></code></pre></td>
						
						</tr>
					</tbody>
				</table>
			
		</div>
	</div>
</div>

<script>
function submitDeleteForm() {
    var message = prompt("delete_confirm_message\n\ndelete_commit_summary", "Delete ''");
    if (message != null) {
        $("#delete-message").val(message);
        $("#delete-file-form").submit()
    }
}
</script>

		
	</div>
</div>

	</div>
	<footer>
		<div class="ui container">
			<div class="ui left">
				© Gitea  
			</div>
			<div class="ui right links">
				
				<div class="ui language bottom floating slide up dropdown link item">
					<i class="world icon"></i>
					<div class="text">English</div>
					<div class="menu">
						
							<a class="item active selected" href="#">English</a>
						
							<a class="item " href="/yorhel/vndb/src/branch/master/util/sql/schema.sql?lang=zh-CN">简体中文</a>
						
							<a class="item " href="/yorhel/vndb/src/branch/master/util/sql/schema.sql?lang=zh-HK">繁體中文（香港）</a>
						
							<a class="item " href="/yorhel/vndb/src/branch/master/util/sql/schema.sql?lang=zh-TW">繁體中文（台灣）</a>
						
							<a class="item " href="/yorhel/vndb/src/branch/master/util/sql/schema.sql?lang=de-DE">Deutsch</a>
						
							<a class="item " href="/yorhel/vndb/src/branch/master/util/sql/schema.sql?lang=fr-FR">Français</a>
						
							<a class="item " href="/yorhel/vndb/src/branch/master/util/sql/schema.sql?lang=nl-NL">Nederlands</a>
						
							<a class="item " href="/yorhel/vndb/src/branch/master/util/sql/schema.sql?lang=lv-LV">Latviešu</a>
						
							<a class="item " href="/yorhel/vndb/src/branch/master/util/sql/schema.sql?lang=ru-RU">Русский</a>
						
							<a class="item " href="/yorhel/vndb/src/branch/master/util/sql/schema.sql?lang=ja-JP">日本語</a>
						
							<a class="item " href="/yorhel/vndb/src/branch/master/util/sql/schema.sql?lang=es-ES">Español</a>
						
							<a class="item " href="/yorhel/vndb/src/branch/master/util/sql/schema.sql?lang=pt-BR">Português do Brasil</a>
						
							<a class="item " href="/yorhel/vndb/src/branch/master/util/sql/schema.sql?lang=pl-PL">polski</a>
						
							<a class="item " href="/yorhel/vndb/src/branch/master/util/sql/schema.sql?lang=bg-BG">български</a>
						
							<a class="item " href="/yorhel/vndb/src/branch/master/util/sql/schema.sql?lang=it-IT">Italiano</a>
						
							<a class="item " href="/yorhel/vndb/src/branch/master/util/sql/schema.sql?lang=fi-FI">Suomalainen</a>
						
							<a class="item " href="/yorhel/vndb/src/branch/master/util/sql/schema.sql?lang=tr-TR">Türkçe</a>
						
							<a class="item " href="/yorhel/vndb/src/branch/master/util/sql/schema.sql?lang=cs-CZ">čeština</a>
						
							<a class="item " href="/yorhel/vndb/src/branch/master/util/sql/schema.sql?lang=sr-SP">Српски</a>
						
							<a class="item " href="/yorhel/vndb/src/branch/master/util/sql/schema.sql?lang=sv-SE">Svenska</a>
						
							<a class="item " href="/yorhel/vndb/src/branch/master/util/sql/schema.sql?lang=ko-KR">한국어</a>
						
					</div>
				</div>
				<a href="/vendor/librejs.html" data-jslicense="1">Javascript licenses</a>
				<a href="/api/swagger">API</a>
				<a target="_blank" rel="noopener" href="https://gitea.io">Website</a>
				
			</div>
		</div>
	</footer>
	<script src="/vendor/plugins/jquery/jquery.min.js"></script>
	<script src="/vendor/plugins/jquery.areyousure/jquery.are-you-sure.js"></script>





	<script src="/vendor/plugins/highlight/highlight.pack.js"></script>




	<script src="/vendor/plugins/autolink/autolink.js"></script>
	<script src="/vendor/plugins/emojify/emojify.min.js"></script>
	<script src="/vendor/plugins/clipboard/clipboard.min.js"></script>
	<script src="/vendor/plugins/vue/vue.min.js"></script>

	
	<script src="/vendor/plugins/semantic/semantic.min.js"></script>
	<script src="/js/index.js?v=38dfe6388acfa83c3486d41f22b08165"></script>
</body>
</html>

