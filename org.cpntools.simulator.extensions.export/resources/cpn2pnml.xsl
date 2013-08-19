<?xml version="1.0" encoding="UTF-8"?>
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" version="1.0" xmlns:pnml="http://www.pnml.org/version-2009/grammar/pnml" xmlns:cpn="http://www.daimi.au.dk/CPNTools/hlpn">

	<!-- TODO
		- Port/Socket for In
		- Handle I/O ports
		- Arc expressions
	-->

	<xsl:variable name="lcletters">abcdefghijklmnopqrstuvwxyz</xsl:variable>
	<xsl:variable name="ucletters">ABCDEFGHIJKLMNOPQRSTUVWXYZ</xsl:variable>

	<xsl:template name="tolower">
		<xsl:param name="toconvert"/>
		<xsl:value-of select="translate($toconvert,$ucletters,$lcletters)"/>
	</xsl:template>

    <!--HV: Convert CPN color names to RGB values. -->
    <xsl:template name="toRGB">
        <xsl:param name="toconvert"/>
        <xsl:variable name="lccolor">
            <xsl:value-of select="translate($toconvert,$ucletters,$lcletters)"/>
        </xsl:variable>
        <xsl:choose>
            <xsl:when test="$lccolor = 'black'">
                <xsl:text>#000000</xsl:text>
            </xsl:when>
            <xsl:when test="$lccolor = 'green'">
                <xsl:text>#008000</xsl:text>
            </xsl:when>
            <xsl:when test="$lccolor = 'silver'">
                <xsl:text>#C0C0C0</xsl:text>
            </xsl:when>
            <xsl:when test="$lccolor = 'lime'">
                <xsl:text>#00FF00</xsl:text>
            </xsl:when>
            <xsl:when test="$lccolor = 'gray'">
                <xsl:text>#808080</xsl:text>
            </xsl:when>
            <xsl:when test="$lccolor = 'olive'">
                <xsl:text>#808000</xsl:text>
            </xsl:when>
            <xsl:when test="$lccolor = 'white'">
                <xsl:text>#FFFFFF</xsl:text>
            </xsl:when>
            <xsl:when test="$lccolor = 'yellow'">
                <xsl:text>#FFFF00</xsl:text>
            </xsl:when>
            <xsl:when test="$lccolor = 'maroon'">
                <xsl:text>#800000</xsl:text>
            </xsl:when>
            <xsl:when test="$lccolor = 'navy'">
                <xsl:text>#000080</xsl:text>
            </xsl:when>
            <xsl:when test="$lccolor = 'red'">
                <xsl:text>#FF0000</xsl:text>
            </xsl:when>
            <xsl:when test="$lccolor = 'blue'">
                <xsl:text>#0000FF</xsl:text>
            </xsl:when>
            <xsl:when test="$lccolor = 'purple'">
                <xsl:text>#800080</xsl:text>
            </xsl:when>
            <xsl:when test="$lccolor = 'teal'">
                <xsl:text>#008080</xsl:text>
            </xsl:when>
            <xsl:when test="$lccolor = 'fucia'">
                <xsl:text>#FF00FF</xsl:text>
            </xsl:when>
            <xsl:when test="$lccolor = 'aqua'">
                <xsl:text>#00FFFF</xsl:text>
            </xsl:when>
            <xsl:otherwise>
                <xsl:text>#000000</xsl:text>
            </xsl:otherwise>
        </xsl:choose>
    </xsl:template>
    
	<xsl:template match="workspaceElements">
		<pnml:pnml xmlns="http://www.daimi.au.dk/CPnets/pnml/hlpn">
			<xsl:apply-templates select="cpnet"/>
            </pnml:pnml>
	</xsl:template>

	<xsl:template match="cpnet">
		<pnml:net id="{generate-id()}" type="http://www.daimi.au.dk/CPnets/pnml/hlpn">
			<xsl:apply-templates select="globbox|page[count(place/port) = 0]"/>
		</pnml:net>
		<xsl:apply-templates select="page[place/port]"/>
		<xsl:apply-templates select="fusion"/>
	</xsl:template>

	<xsl:template match="globbox">
		<pnml:declarations>
			<xsl:apply-templates select="block|color" mode="decl"/>
		</pnml:declarations>
	</xsl:template>

	<xsl:template match="block" mode="decl">
		<pnml:toolspecific tool="CPN Tools" version="1.0.0">
			<cpn:startblock>
				<xsl:attribute name="name"><xsl:value-of select="id"/></xsl:attribute>
			</cpn:startblock>
		</pnml:toolspecific>
		<xsl:apply-templates select="color" mode="decl"/>
		<pnml:toolspecific tool="CPN Tools" version="1.0.0">
			<cpn:endblock/>
		</pnml:toolspecific>
	</xsl:template>

	<xsl:template match="color" mode="decl">
		<pnml:declaration>
			<pnml:type>
				<xsl:apply-templates select="id" mode="decl"/>
				<pnml:type>
					<xsl:apply-templates select="int|bool|string|enum|product" mode="decl"/>
				</pnml:type>
			</pnml:type>
		</pnml:declaration>
	</xsl:template>

	<xsl:template match="color/id" mode="decl">
		<pnml:name>
			<xsl:value-of select="text()"/>
		</pnml:name>
	</xsl:template>

	<xsl:template match="int" mode="decl"><pnml:int/></xsl:template>
	<xsl:template match="bool" mode="decl"><pnml:bool/></xsl:template>
	<xsl:template match="string" mode="decl"><pnml:string/></xsl:template>

	<xsl:template match="enum" mode="decl">
		<pnml:enum>
			<xsl:apply-templates select="node()|text()" mode="decl"/>
		</pnml:enum>
	</xsl:template>

	<xsl:template match="product" mode="decl">
		<pnml:product>
			<xsl:for-each select="id">
				<pnml:type>
					<pnml:text>
						<xsl:value-of select="text()"/>
					</pnml:text>
				</pnml:type>
			</xsl:for-each>
		</pnml:product>
	</xsl:template>

	<xsl:template match="enum/id" mode="decl">
		<pnml:text>
			<xsl:value-of select="text()"/>
		</pnml:text>
	</xsl:template>

	<xsl:template match="page" priority="1">
		<pnml:page><xsl:attribute name="id"><xsl:value-of select="@id"/></xsl:attribute>
			<xsl:apply-templates select="node()|text()"/>
		</pnml:page>
	</xsl:template>

	<xsl:template match="page[place/port]" priority="2">
		<pnml:module><xsl:attribute name="id"><xsl:value-of select="@id"/></xsl:attribute>
			<pnml:interface>
				<xsl:apply-templates select="place[port]" mode="import"/>
<!--				<xsl:apply-templates select="place[port/@type='In']" mode="export"/>-->
			</pnml:interface>
			<xsl:apply-templates select="node()|text()"/>
		</pnml:module>
	</xsl:template>

	<xsl:template match="pageattr">
		<pnml:name>
			<pnml:text>
				<xsl:value-of select="@name"/>
			</pnml:text>
		</pnml:name>
	</xsl:template>

	<xsl:template match="place" priority="1">
		<pnml:place><xsl:attribute name="id"><xsl:value-of select="@id"/></xsl:attribute>
			<xsl:apply-templates select="text"/>
			<xsl:apply-templates select="initmark"/>
			<xsl:apply-templates select="type"/>
			<pnml:graphics>
				<xsl:apply-templates select="posattr|fillattr|lineattr|ellipse|box" mode="graphics"/>
			</pnml:graphics>
		</pnml:place>
	</xsl:template>

	<xsl:template match="place[port]" priority="2">
		<pnml:referencePlace ref="{generate-id()}">
			<xsl:attribute name="id"><xsl:value-of select="@id"/></xsl:attribute>
			<pnml:graphics>
				<xsl:apply-templates select="posattr|fillattr|lineattr|ellipse|box" mode="graphics"/>
			</pnml:graphics>
		</pnml:referencePlace>
	</xsl:template>

	<xsl:template match="place[fusioninfo]" priority="2">
		<xsl:variable name="fusname"><xsl:value-of select="fusioninfo/@name"/></xsl:variable>
		<pnml:referencePlace gref="{generate-id(//fusion[@name=$fusname])}">
			<xsl:attribute name="id"><xsl:value-of select="@id"/></xsl:attribute>
			<pnml:graphics>
				<xsl:apply-templates select="posattr|fillattr|lineattr|ellipse|box" mode="graphics"/>
			</pnml:graphics>
		</pnml:referencePlace>
	</xsl:template>

	<xsl:template match="place" priority="1" mode="fusion">
		<xsl:variable name="fusiongroup"><xsl:value-of select="@id"/></xsl:variable>
		<pnml:globalPlace id="{generate-id(//fusion/fusion_elm[@idref=$fusiongroup])}">
			<xsl:apply-templates select="text"/>
			<xsl:apply-templates select="initmark"/>
			<xsl:apply-templates select="type"/>
			<pnml:graphics>
				<xsl:apply-templates select="posattr|fillattr|lineattr|ellipse|box" mode="graphics"/>
			</pnml:graphics>
		</pnml:globalPlace>
	</xsl:template>

<!--	<xsl:template match="place[port]" priority="2" mode="fusion">
		<pnml:referencePlace ref="{generate-id()}">
			<xsl:attribute name="id"><xsl:value-of select="@id"/></xsl:attribute>
			<pnml:graphics>
				<xsl:apply-templates select="posattr|fillattr|lineattr|ellipse|box" mode="graphics"/>
			</pnml:graphics>
		</pnml:referencePlace>
	</xsl:template>-->

	<xsl:template match="place" mode="import">
		<pnml:importPlace id="{generate-id()}"/>
	</xsl:template>

	<xsl:template match="place" mode="export">
		<pnml:exportPlace id="{generate-id()}">
			<xsl:attribute name="ref"><xsl:value-of select="@id"/></xsl:attribute>
			<xsl:apply-templates select="text"/>
			<xsl:apply-templates select="initmark"/>
			<xsl:apply-templates select="type"/>
		</pnml:exportPlace>
	</xsl:template>

	<xsl:template match="trans" priority="1">
		<pnml:transition><xsl:attribute name="id"><xsl:value-of select="@id"/></xsl:attribute>
			<xsl:apply-templates select="text"/>
			<xsl:apply-templates select="cond"/>
			<pnml:toolspecific tool="CPN Tools" version="1.0.0">
				<xsl:apply-templates select="time"/>
				<xsl:apply-templates select="code"/>
			</pnml:toolspecific>
			<pnml:graphics>
				<xsl:apply-templates select="posattr|fillattr|lineattr|ellipse|box" mode="graphics"/>
			</pnml:graphics>
		</pnml:transition>
	</xsl:template>

	<xsl:template match="Aux" priority="1">
		<pnml:toolspecific tool="CPN Tools" version="1.0.0">
			<cpn:aux><xsl:attribute name="id"><xsl:value-of select="@id"/></xsl:attribute>
				<cpn:graphics>
					<xsl:apply-templates select="posattr|fillattr|lineattr|ellipse|box" mode="cpngraphics"/>
				</cpn:graphics>
			</cpn:aux>
		</pnml:toolspecific>
	</xsl:template>

	<xsl:template match="Aux[text]" priority="2">
		<pnml:toolspecific tool="CPN Tools" version="1.0.0">
			<cpn:aux><xsl:attribute name="id"><xsl:value-of select="@id"/></xsl:attribute>
				<xsl:apply-templates select="text" mode="cpngraphics"/>
				<cpn:graphics>
					<xsl:apply-templates select="posattr|textattr" mode="cpngraphics"/>
				</cpn:graphics>
			</cpn:aux>
		</pnml:toolspecific>
	</xsl:template>

	<xsl:template name="writePortSocket">
		<xsl:param name="assignments"/>
		<xsl:if test="not($assignments = '')">
			<xsl:variable name="assignment"><xsl:value-of select="substring-before($assignments, ')')"/></xsl:variable>
			<xsl:variable name="first"><xsl:value-of select="substring-after(substring-before($assignment, ','), '(')"/></xsl:variable>
			<xsl:variable name="second"><xsl:value-of select="substring-after($assignment, ',')"/></xsl:variable>
			<xsl:if test="//place[@id=$first]/port">
				<pnml:importPlace>
					<xsl:attribute name="parameter"><xsl:value-of select="generate-id(//place[@id=$first])"/></xsl:attribute>
					<xsl:attribute name="ref"><xsl:value-of select="$second"/></xsl:attribute>
				</pnml:importPlace>
			</xsl:if>
			<xsl:call-template name="writePortSocket">
				<xsl:with-param name="assignments" select="substring-after($assignments, ')')"/>
			</xsl:call-template>
		</xsl:if>
	</xsl:template>

    <!--HV: replaced pnml:instance by pnml:transition for the time being. -->
	<xsl:template match="trans[subst]" priority="2">
		<pnml:transition>
			<xsl:attribute name="id"><xsl:value-of select="@id"/></xsl:attribute>
			<xsl:attribute name="ref"><xsl:value-of select="subst/@subpage"/></xsl:attribute>
			<xsl:apply-templates select="text"/>
			<xsl:call-template name="writePortSocket"><xsl:with-param name="assignments" select="subst/@portsock"/></xsl:call-template>
			<pnml:graphics>
				<xsl:apply-templates select="posattr|fillattr|lineattr|ellipse|box" mode="graphics"/>
			</pnml:graphics>
		</pnml:transition>
	</xsl:template>

	<xsl:template name="arccommon">
		<xsl:apply-templates select="annot"/>
	</xsl:template>
	<xsl:template name="arccommonTP">
		<xsl:attribute name="id"><xsl:value-of select="concat(@id,'tp')"/></xsl:attribute>
		<xsl:call-template name="arccommon"/>
		<pnml:graphics>
			<xsl:apply-templates select="lineattr" mode="graphics"/>
			<xsl:for-each select="bendpoint">
				<xsl:sort select="@serial" order="ascending" data-type="number"/>
				<xsl:apply-templates select="posattr" mode="graphics"/>
			</xsl:for-each>
		</pnml:graphics>
	</xsl:template>
	<xsl:template name="arccommonPT">
		<xsl:attribute name="id"><xsl:value-of select="concat(@id,'pt')"/></xsl:attribute>
		<xsl:call-template name="arccommon"/>
		<pnml:graphics>
			<xsl:apply-templates select="lineattr" mode="graphics"/>
			<xsl:for-each select="bendpoint">
				<xsl:sort select="@serial" order="descending" data-type="number"/>
				<xsl:apply-templates select="posattr" mode="graphics"/>
			</xsl:for-each>
		</pnml:graphics>
	</xsl:template>

	<xsl:template match="arc[@orientation='PtoT']">
		<pnml:arc>
			<xsl:attribute name="source"><xsl:value-of select="placeend/@idref"/></xsl:attribute>
			<xsl:attribute name="target"><xsl:value-of select="transend/@idref"/></xsl:attribute>
			<xsl:call-template name="arccommonPT"/>
			<pnml:arctype><pnml:text>normal</pnml:text></pnml:arctype>
		</pnml:arc>
	</xsl:template>

	<xsl:template match="arc[@orientation='TtoP']">
		<pnml:arc>
			<xsl:attribute name="source"><xsl:value-of select="transend/@idref"/></xsl:attribute>
			<xsl:attribute name="target"><xsl:value-of select="placeend/@idref"/></xsl:attribute>
			<xsl:call-template name="arccommonTP"/>
			<pnml:arctype><pnml:text>normal</pnml:text></pnml:arctype>
		</pnml:arc>
	</xsl:template>
	<xsl:template match="arc[@orientation='Reset']">
		<pnml:arc>
			<xsl:attribute name="source"><xsl:value-of select="transend/@idref"/></xsl:attribute>
			<xsl:attribute name="target"><xsl:value-of select="placeend/@idref"/></xsl:attribute>
			<xsl:call-template name="arccommonTP"/>
			<pnml:arctype><pnml:text>reset</pnml:text></pnml:arctype>
		</pnml:arc>
	</xsl:template>
	<xsl:template match="arc[@orientation='Inhibitor']">
		<pnml:arc>
			<xsl:attribute name="source"><xsl:value-of select="transend/@idref"/></xsl:attribute>
			<xsl:attribute name="target"><xsl:value-of select="placeend/@idref"/></xsl:attribute>
			<xsl:call-template name="arccommonTP"/>
			<pnml:arctype><pnml:text>inhibitor</pnml:text></pnml:arctype>
		</pnml:arc>
	</xsl:template>

    <!--HV: Split bidirectional arc into two unidirectional arcs. -->
	<xsl:template match="arc[@orientation='BOTHDIR']">
		<pnml:arc>
			<xsl:attribute name="source"><xsl:value-of select="transend/@idref"/></xsl:attribute>
			<xsl:attribute name="target"><xsl:value-of select="placeend/@idref"/></xsl:attribute>
			<xsl:call-template name="arccommonTP"/>
			<pnml:arctype><pnml:text>normal</pnml:text></pnml:arctype>
		</pnml:arc>
		<pnml:arc>
			<xsl:attribute name="source"><xsl:value-of select="placeend/@idref"/></xsl:attribute>
			<xsl:attribute name="target"><xsl:value-of select="transend/@idref"/></xsl:attribute>
			<xsl:call-template name="arccommonPT"/>
			<pnml:arctype><pnml:text>normal</pnml:text></pnml:arctype>
		</pnml:arc>
	</xsl:template>

	<xsl:template match="place/text|trans/text">
		<pnml:name>
			<pnml:text>
				<xsl:value-of select="text()"/>
			</pnml:text>
			<pnml:graphics>
				<pnml:offset x="0" y="0"/>
				<pnml:line>
					<xsl:attribute name="color">
						<xsl:call-template name="toRGB">
							<xsl:with-param name="toconvert" select="../textattr/@colour"/>
						</xsl:call-template>
					</xsl:attribute>
				</pnml:line>
				<pnml:font family="arial" size="12" style="normal" align="left">
					<xsl:attribute name="weight">
						<xsl:choose>
							<xsl:when test="translate(../textattr/@bold,$ucletters,$lcletters)='false'">normal</xsl:when>
							<xsl:otherwise>bold</xsl:otherwise>
						</xsl:choose>
					</xsl:attribute>
				</pnml:font>
			</pnml:graphics>
		</pnml:name>
	</xsl:template>

	<xsl:template match="initmark">
		<xsl:variable name="text"><xsl:value-of select="text"/></xsl:variable>
        <xsl:choose>
	        <xsl:when test="$text = '()'">
				<pnml:initialMarking>
					<pnml:text>1</pnml:text>
					<xsl:call-template name="annotationcommon"/>
				</pnml:initialMarking>
	        </xsl:when>
	        <xsl:when test="string(number(translate($text,'(`)','   '))) != 'NaN'">
				<pnml:initialMarking>
					<pnml:text><xsl:value-of select="normalize-space(translate($text,'(`)','   '))"/></pnml:text>
					<xsl:call-template name="annotationcommon"/>
				</pnml:initialMarking>
	        </xsl:when>
            <xsl:otherwise>
				<pnml:initialMarking>
                    <xsl:comment>
                        <xsl:value-of select="$text"/>
                    </xsl:comment>
					<pnml:text>0</pnml:text>
					<xsl:call-template name="annotationcommon"/>
				</pnml:initialMarking>
            </xsl:otherwise>
        </xsl:choose>
	</xsl:template>

	<xsl:template match="type">
		<pnml:type>
			<pnml:text>
				<xsl:value-of select="text"/>
			</pnml:text>
			<xsl:call-template name="annotationcommon"/>
		</pnml:type>
	</xsl:template>

	<xsl:template match="cond">
		<pnml:guard>
			<pnml:text>
				<xsl:value-of select="text"/>
			</pnml:text>
			<xsl:call-template name="annotationcommon"/>
		</pnml:guard>
	</xsl:template>

	<xsl:template match="time">
		<cpn:time>
			<xsl:call-template name="cpnannotationcommon"/>
		</cpn:time>
	</xsl:template>

	<xsl:template match="code">
		<cpn:code>
			<xsl:call-template name="cpnannotationcommon"/>
		</cpn:code>
	</xsl:template>

	<xsl:template match="annot">
		<pnml:inscription>
			<xsl:variable name="text">
				<xsl:value-of select="text" />
			</xsl:variable>
			<xsl:choose>
				<xsl:when test="$text = '()'">
					<pnml:text>1</pnml:text>
				</xsl:when>
				<xsl:when test="$text = ''">
					<pnml:text>1</pnml:text>
				</xsl:when>
				<xsl:when test="string(number(translate($text,'(`)','   '))) != 'NaN'">
					<pnml:text>
						<xsl:value-of select="normalize-space(translate($text,'(`)','   '))" />
					</pnml:text>
				</xsl:when>
				<xsl:otherwise>
					<xsl:comment>
						<xsl:value-of select="$text" />
					</xsl:comment>
					<pnml:text>0</pnml:text>
				</xsl:otherwise>
			</xsl:choose>
			<xsl:call-template name="annotationcommon" />
		</pnml:inscription>
	</xsl:template>

	<xsl:template name="annotationcommon">
		<pnml:graphics>
			<pnml:offset>
				<xsl:attribute name="x">
					<xsl:value-of select="posattr/@x - ../posattr/@x"/>
				</xsl:attribute>
				<xsl:attribute name="y">
					<xsl:value-of select="posattr/@y - ../posattr/@y"/>
				</xsl:attribute>
			</pnml:offset>
			<pnml:line>
				<xsl:attribute name="color">
					<xsl:call-template name="toRGB">
						<xsl:with-param name="toconvert" select="textattr/@colour"/>
					</xsl:call-template>
				</xsl:attribute>
			</pnml:line>
			<pnml:font family="arial" size="12" style="normal" align="left">
				<xsl:attribute name="weight">
					<xsl:choose>
						<xsl:when test="translate(textattr/@bold,$ucletters,$lcletters)='false'">normal</xsl:when>
						<xsl:otherwise>bold</xsl:otherwise>
					</xsl:choose>
				</xsl:attribute>
			</pnml:font>
		</pnml:graphics>
	</xsl:template>

	<xsl:template name="cpnannotationcommon">
		<xsl:apply-templates select="text" mode="cpngraphics"/>
		<cpn:graphics>
			<xsl:apply-templates select="textattr" mode="cpngraphics"/>
			<cpn:offset>
				<xsl:attribute name="x">
					<xsl:value-of select="posattr/@x - ../posattr/@x"/>
				</xsl:attribute>
				<xsl:attribute name="y">
					<xsl:value-of select="posattr/@y - ../posattr/@y"/>
				</xsl:attribute>
			</cpn:offset>
		</cpn:graphics>
	</xsl:template>

	<xsl:template match="posattr" mode="cpngraphics">
		<cpn:position>
			<xsl:attribute name="x"><xsl:value-of select="@x"/></xsl:attribute>
			<xsl:attribute name="y"><xsl:value-of select="@y"/></xsl:attribute>
		</cpn:position>
	</xsl:template>

	<xsl:template match="fillattr[@filled='true']" mode="cpngraphics">
		<cpn:fill>
			<xsl:attribute name="color">
				<xsl:call-template name="tolower">
					<xsl:with-param name="toconvert" select="@colour"/>
				</xsl:call-template>
			</xsl:attribute>
		</cpn:fill>
	</xsl:template>

	<xsl:template match="fillattr" mode="cpngraphics" priority="1">
		<cpn:fill>
			<xsl:attribute name="color">
				<xsl:call-template name="tolower">
					<xsl:with-param name="toconvert" select="@colour"/>
				</xsl:call-template>
			</xsl:attribute>
		</cpn:fill>
	</xsl:template>

	<xsl:template match="fillattr[@filled='false']" mode="cpngraphics" priority="2">
		<cpn:fill color="white"/>
	</xsl:template>

	<xsl:template match="lineattr" mode="cpngraphics">
		<cpn:line>
			<xsl:attribute name="color">
				<xsl:call-template name="tolower">
					<xsl:with-param name="toconvert" select="@colour"/>
				</xsl:call-template>
			</xsl:attribute>
			<xsl:attribute name="width">
                <!--HV: Add 1 to line thickness, as 0 results in an invisible line. -->
				<xsl:value-of select="1 + @thick"/>
			</xsl:attribute>
			<xsl:attribute name="style">
				<xsl:call-template name="tolower">
					<xsl:with-param name="toconvert" select="@type"/>
				</xsl:call-template>
			</xsl:attribute>
		</cpn:line>
	</xsl:template>

	<xsl:template match="textattr" mode="cpngraphics">
		<cpn:line>
			<xsl:attribute name="color">
				<xsl:call-template name="tolower">
					<xsl:with-param name="toconvert" select="textattr/@colour"/>
				</xsl:call-template>
			</xsl:attribute>
		</cpn:line>
		<cpn:font family="arial" size="12" style="normal" align="left">
			<xsl:attribute name="weight">
				<xsl:choose>
					<xsl:when test="translate(textattr/@bold,$ucletters,$lcletters)='false'">normal</xsl:when>
					<xsl:otherwise>bold</xsl:otherwise>
				</xsl:choose>
			</xsl:attribute>
		</cpn:font>
	</xsl:template>

	<xsl:template match="text" mode="cpngraphics">
		<cpn:text>
			<xsl:value-of select="text()"/>
		</cpn:text>
	</xsl:template>

	<xsl:template match="ellipse" mode="cpngraphics">
		<cpn:dimension>
			<xsl:attribute name="x"><xsl:value-of select="@w"/></xsl:attribute>
			<xsl:attribute name="y"><xsl:value-of select="@h"/></xsl:attribute>
		</cpn:dimension>
		<cpn:ellipse/>
	</xsl:template>

	<xsl:template match="box" mode="cpngraphics">
		<cpn:dimension>
			<xsl:attribute name="x"><xsl:value-of select="@w"/></xsl:attribute>
			<xsl:attribute name="y"><xsl:value-of select="@h"/></xsl:attribute>
		</cpn:dimension>
		<cpn:rectangle/>
	</xsl:template>

    <!--HV: Calculate the minimal value out of a colleciton of values.-->
    <xsl:template name="minvalue">
        <xsl:param name="values"/>
        <xsl:param name="min"/>
        <xsl:choose>
            <xsl:when test="number($values[position()=1]) &lt; number($min)">
                <xsl:call-template name="minvalue">
                    <xsl:with-param name="values" select="$values[position()!=1]"/>
                    <xsl:with-param name="min" select="$values[position()=1]"/>
                </xsl:call-template>
            </xsl:when>
            <xsl:when test="number($values[position()=1]) &gt;= number($min)">
                <xsl:call-template name="minvalue">
                    <xsl:with-param name="values" select="$values[position()!=1]"/>
                    <xsl:with-param name="min" select="$min"/>
                </xsl:call-template>
            </xsl:when>
            <xsl:otherwise>
                <xsl:value-of select="$min"/>
            </xsl:otherwise>
        </xsl:choose>
    </xsl:template>
    
    <!--HV: Calculate the maximal value out of a colleciton of values.-->
    <xsl:template name="maxvalue">
        <xsl:param name="values"/>
        <xsl:param name="max"/>
        <xsl:choose>
            <xsl:when test="number($values[position()=1]) &gt; number($max)">
                <xsl:call-template name="maxvalue">
                    <xsl:with-param name="values" select="$values[position()!=1]"/>
                    <xsl:with-param name="max" select="$values[position()=1]"/>
                </xsl:call-template>
            </xsl:when>
            <xsl:when test="number($values[position()=1]) &lt;= number($max)">
                <xsl:call-template name="maxvalue">
                    <xsl:with-param name="values" select="$values[position()!=1]"/>
                    <xsl:with-param name="max" select="$max"/>
                </xsl:call-template>
            </xsl:when>
            <xsl:otherwise>
                <xsl:value-of select="$max"/>
            </xsl:otherwise>
        </xsl:choose>
    </xsl:template>
    
	<xsl:template match="posattr" mode="graphics">
        <!--HV: Determine minimal x coordinate and maximal y coordinate. -->
        <xsl:variable name="xmin">
            <xsl:call-template name="minvalue">
                <xsl:with-param name="values" select="/workspaceElements//posattr/@x"/>
                <xsl:with-param name="min" select="0"/>
            </xsl:call-template>
        </xsl:variable>
        <xsl:variable name="ymax">
            <xsl:call-template name="maxvalue">
                <xsl:with-param name="values" select="/workspaceElements//posattr/@y"/>
                <xsl:with-param name="max" select="0"/>
            </xsl:call-template>
        </xsl:variable>
        <xsl:variable name="wmax">
            <xsl:call-template name="maxvalue">
                <xsl:with-param name="values" select="/workspaceElements//ellipse/@w | /workspaceElements//box/@w"/>
                <xsl:with-param name="max" select="0"/>
            </xsl:call-template>
        </xsl:variable>
        <xsl:variable name="hmax">
            <xsl:call-template name="maxvalue">
                <xsl:with-param name="values" select="/workspaceElements//ellipse/@h | /workspaceElements//box/@h"/>
                <xsl:with-param name="max" select="0"/>
            </xsl:call-template>
        </xsl:variable>
		<pnml:position>
            <!--HV: Compute new coordinates (invert y, use (1 + half max width, 1 + half max height) as offset) -->
			<xsl:attribute name="x"><xsl:value-of select="1 + $wmax div 2 + @x - $xmin"/></xsl:attribute>
			<xsl:attribute name="y"><xsl:value-of select="1 + $hmax div 2 + $ymax - @y"/></xsl:attribute>
		</pnml:position>
	</xsl:template>

	<xsl:template match="fillattr[@filled='true']" mode="graphics">
		<pnml:fill>
			<xsl:attribute name="color">
				<xsl:call-template name="toRGB">
					<xsl:with-param name="toconvert" select="@colour"/>
				</xsl:call-template>
			</xsl:attribute>
		</pnml:fill>
	</xsl:template>

	<xsl:template match="place/fillattr|trans/fillattr" mode="graphics" priority="1">
		<pnml:fill>
			<xsl:attribute name="color">
				<xsl:call-template name="toRGB">
					<xsl:with-param name="toconvert" select="@colour"/>
				</xsl:call-template>
			</xsl:attribute>
		</pnml:fill>
	</xsl:template>

	<xsl:template match="place/fillattr[@filled='false']|trans|fillattr[@filled='false']" mode="graphics" priority="2">
		<pnml:fill color="#ffffff"/>
	</xsl:template>

	<xsl:template match="lineattr" mode="graphics">
		<xsl:variable name="style">
            <xsl:call-template name="tolower">
				<xsl:with-param name="toconvert" select="@type"/>
		    </xsl:call-template>
        </xsl:variable>
		<pnml:line>
			<xsl:attribute name="color">
				<xsl:call-template name="toRGB">
					<xsl:with-param name="toconvert" select="@colour"/>
				</xsl:call-template>
			</xsl:attribute>
			<xsl:attribute name="width">
				<xsl:value-of select="1 + @thick"/>
			</xsl:attribute>
			<xsl:attribute name="style">
				<xsl:choose>
					<xsl:when test="$style='dashed'">dash</xsl:when>
					<xsl:when test="@style='dotted'">dot</xsl:when>
					<xsl:otherwise>solid</xsl:otherwise> <!-- this uncludes when @type = 'solid' -->
				</xsl:choose>
			</xsl:attribute>
		</pnml:line>
		<xsl:if test="not($style='solid' or $style='dashed' or $style='dotted')">
			<pnml:toolspecific tool="CPN Tools" version="1.0.0">
				<cpn:line>
					<xsl:attribute name="color">
						<xsl:call-template name="tolower">
							<xsl:with-param name="toconvert" select="@colour"/>
						</xsl:call-template>
					</xsl:attribute>
					<xsl:attribute name="width">
						<xsl:value-of select="@thick"/>
					</xsl:attribute>
					<xsl:attribute name="style">
						<xsl:call-template name="tolower">
							<xsl:with-param name="toconvert" select="@type"/>
						</xsl:call-template>
					</xsl:attribute>
				</cpn:line>
			</pnml:toolspecific>
		</xsl:if>
	</xsl:template>

	<xsl:template match="ellipse" mode="graphics">
		<pnml:dimension>
			<xsl:attribute name="x"><xsl:value-of select="@w"/></xsl:attribute>
			<xsl:attribute name="y"><xsl:value-of select="@h"/></xsl:attribute>
		</pnml:dimension>
		<pnml:toolspecific tool="CPN Tools" version="1.0.0">
			<cpn:ellipse/>
		</pnml:toolspecific>
	</xsl:template>

	<xsl:template match="box" mode="graphics">
		<pnml:dimension>
			<xsl:attribute name="x"><xsl:value-of select="@w"/></xsl:attribute>
			<xsl:attribute name="y"><xsl:value-of select="@h"/></xsl:attribute>
		</pnml:dimension>
		<pnml:toolspecific tool="CPN Tools" version="1.0.0">
			<cpn:rectangle/>
		</pnml:toolspecific>
	</xsl:template>

	<xsl:template match="fusion">
		<pnml:module><xsl:attribute name="id"><xsl:value-of select="@id"/></xsl:attribute>
			<xsl:variable name="representant"><xsl:value-of select="fusion_elm[1]/@idref"/></xsl:variable>
			<pnml:interface>
				<exportPlace id="{generate-id()}" gref="{generate-id(fusion_elm[1])}">
					<xsl:apply-templates select="//place[@id=$representant]/text"/>
					<xsl:apply-templates select="//place[@id=$representant]/initmark"/>
					<xsl:apply-templates select="//place[@id=$representant]/type"/>
				</exportPlace>
			</pnml:interface>
			<pnml:name><pnml:text><xsl:value-of select="@name"/></pnml:text></pnml:name>
			<xsl:apply-templates select="//place[@id=$representant]" mode="fusion"/>
		</pnml:module>
	</xsl:template>

</xsl:stylesheet>
