<?php require_once('access.php');?>

<!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 4.01 Transitional//EN"
		"http://www.w3.org/TR/html4/loose.dtd">
<html>
<head>
	<meta http-equiv="Content-Type" content="text/html; charset=utf-8">
    <title>View OpenLCB Unique ID Ranges</title>
    
    <STYLE TYPE="text/css">
    </STYLE>

</head>
<body>
<IMG SRC="logo-ajs-dph.png" NAME="graphics1" ALIGN=RIGHT WIDTH=195 HEIGHT=80 BORDER=0>
<h1>View OpenLCB Unique ID Ranges</h1>  

This page shows the ranges of OpenLCB Unique ID's that have been assigned to date.
The numbers below are in hexadecimal.
<P>
For more information on OpenLCB, please see the <a href="../documents/index.html">documentation page</a>.
For more information on OpenLCB unique ID assignment, please see the current draft
<a href="../specs/drafts/GenUniqueIdS.pdf">specification</a> and 
<a href="../specs/drafts/GenUniqueIdTN.pdf">technical note</a>.

<?php 

// open DB
global $opts;
mysql_connect($opts['hn'],$opts['un'],$opts['pw']);
@mysql_select_db($opts['db']) or die( "Unable to select database. Error (" . mysql_errno() . ") " . mysql_error());


function value($result, $j, $index) {
    if (255 == mysql_result($result,$j,"uniqueid_byte".$index."_mask")) return "*";
    else return strtoupper(dechex(mysql_result($result,$j,"uniqueid_byte".$index."_value")));
}

$query = "SELECT * FROM UniqueIDs LEFT JOIN Person USING (person_id)
        WHERE   uniqueid_byte1_mask = 255
            AND uniqueid_byte2_mask = 255
            AND uniqueid_byte3_mask = 255
            AND uniqueid_byte4_mask = 255
            AND uniqueid_byte5_mask = 255            
        ORDER BY uniqueid_byte0_value, uniqueid_byte0_mask, 
                uniqueid_byte1_value, uniqueid_byte1_mask,
                uniqueid_byte2_value, uniqueid_byte2_mask,
                uniqueid_byte3_value, uniqueid_byte3_mask,
                uniqueid_byte4_value, uniqueid_byte4_mask,
                uniqueid_byte5_value, uniqueid_byte5_mask
        ;";
$result=mysql_query($query);

echo '<table border="1">';
echo "<tr><th colspan='6'>Range. '*' means that any values are accepted in that byte.</th>";
echo "<th>Delegating organization or person</th><th>URL</th><th>Comment</th></tr>";

for ($j = 0; $j < mysql_numrows($result); $j++) {
    echo '<tr>';
    echo '<td WIDTH="20" ALIGN="CENTER">'.value($result,$j,"0").'</td>';
    echo '<td WIDTH="20" ALIGN="CENTER">'.value($result,$j,"1").'</td>';
    echo '<td WIDTH="20" ALIGN="CENTER">'.value($result,$j,"2").'</td>';
    echo '<td WIDTH="20" ALIGN="CENTER">'.value($result,$j,"3").'</td>';
    echo '<td WIDTH="20" ALIGN="CENTER">'.value($result,$j,"4").'</td>';
    echo '<td WIDTH="20" ALIGN="CENTER">'.value($result,$j,"5").'</td>';
    if (mysql_result($result,$j,"person_organization") != '') {
        echo '<td>'.mysql_result($result,$j,"person_organization").'</td>';
    } else {
        echo '<td>'.mysql_result($result,$j,"person_first_name").' '.mysql_result($result,$j,"person_last_name").'</td>';
    }
    echo '<td>'.mysql_result($result,$j,"uniqueid_url").'</td>';
    echo '<td>'.mysql_result($result,$j,"uniqueid_user_comment").'</td>';
    echo '</tr>';
}

echo '</table>';

for ($i = 0; $i < mysql_numrows($result); $i++) {

    $query = "SELECT * FROM UniqueIDs LEFT JOIN Person USING (person_id)
            WHERE   uniqueid_byte0_value = ".mysql_result($result,$i,"uniqueid_byte0_value")."   
                AND NOT ( uniqueid_byte1_mask = 255 AND uniqueid_byte2_mask = 255 AND uniqueid_byte3_mask = 255 
                        AND uniqueid_byte4_mask = 255 AND uniqueid_byte5_mask = 255 )
            ORDER BY uniqueid_byte1_value, uniqueid_byte1_mask,
                    uniqueid_byte2_value, uniqueid_byte2_mask,
                    uniqueid_byte3_value, uniqueid_byte3_mask,
                    uniqueid_byte4_value, uniqueid_byte4_mask,
                    uniqueid_byte5_value, uniqueid_byte5_mask
            ;";
    $table=mysql_query($query);
    
    if (mysql_numrows($table) > 0) {
        echo "<h3>".mysql_result($result,$i,"uniqueid_user_comment")."</h3>\n";
        echo '<table border="1">';
        echo "<tr><th colspan='6'>Range. '*' means that any values are accepted in that byte.</th>";
        echo "<th>Delegating organization or person</th><th>URL</th><th>Comment</th></tr>";
        
        for ($j = 0; $j < mysql_numrows($table); $j++) {
            echo '<tr>';
            echo '<td WIDTH="20" ALIGN="CENTER">'.value($table,$j,"0").'</td>';
            echo '<td WIDTH="20" ALIGN="CENTER">'.value($table,$j,"1").'</td>';
            echo '<td WIDTH="20" ALIGN="CENTER">'.value($table,$j,"2").'</td>';
            echo '<td WIDTH="20" ALIGN="CENTER">'.value($table,$j,"3").'</td>';
            echo '<td WIDTH="20" ALIGN="CENTER">'.value($table,$j,"4").'</td>';
            echo '<td WIDTH="20" ALIGN="CENTER">'.value($table,$j,"5").'</td>';
            if (mysql_result($table,$j,"person_organization") != '') {
                echo '<td>'.mysql_result($table,$j,"person_organization").'</td>';
            } else {
                echo '<td>'.mysql_result($table,$j,"person_first_name").' '.mysql_result($table,$j,"person_last_name").'</td>';
            }
            echo '<td>'.mysql_result($table,$j,"uniqueid_url").'</td>';
            echo '<td>'.mysql_result($table,$j,"uniqueid_user_comment").'</td>';
            echo '</tr>';
        }
        
        echo '</table>';
    }
}

?>

</body></html>
