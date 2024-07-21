      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Januar 2001
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 2001 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DPKI0007.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/MF/CPY/DPKI0007.CPv  $
      *
      *   Rev 2.1   22 Oct 2001 14:40:40   t13ex27
      *Umstellung von OS2 auf W2000
      *
      *   Rev 2.0   05 Jun 2001 17:32:16   p13sp16
      *Archiverstellung
      *
      *---------------------------------------------------------------*
           MOVE SPACES TO BYTE-FELD-26
           PERFORM VARYING HEX-I FROM 1   BY 1   UNTIL HEX-I > 13
              EVALUATE ID-IN-FELD (HEX-I:1)
                    WHEN X'00' MOVE '00' TO BYTE-FELD-2
                    WHEN X'01' MOVE '01' TO BYTE-FELD-2
                    WHEN X'02' MOVE '02' TO BYTE-FELD-2
                    WHEN X'03' MOVE '03' TO BYTE-FELD-2
                    WHEN X'04' MOVE '04' TO BYTE-FELD-2
                    WHEN X'05' MOVE '05' TO BYTE-FELD-2
                    WHEN X'06' MOVE '06' TO BYTE-FELD-2
                    WHEN X'07' MOVE '07' TO BYTE-FELD-2
                    WHEN X'08' MOVE '08' TO BYTE-FELD-2
                    WHEN X'09' MOVE '09' TO BYTE-FELD-2
                    WHEN X'0A' MOVE '0A' TO BYTE-FELD-2
                    WHEN X'0B' MOVE '0B' TO BYTE-FELD-2
                    WHEN X'0C' MOVE '0C' TO BYTE-FELD-2
                    WHEN X'0D' MOVE '0D' TO BYTE-FELD-2
                    WHEN X'0E' MOVE '0E' TO BYTE-FELD-2
                    WHEN X'0F' MOVE '0F' TO BYTE-FELD-2
                    WHEN X'10' MOVE '10' TO BYTE-FELD-2
                    WHEN X'11' MOVE '11' TO BYTE-FELD-2
                    WHEN X'12' MOVE '12' TO BYTE-FELD-2
                    WHEN X'13' MOVE '13' TO BYTE-FELD-2
                    WHEN X'14' MOVE '14' TO BYTE-FELD-2
                    WHEN X'15' MOVE '15' TO BYTE-FELD-2
                    WHEN X'16' MOVE '16' TO BYTE-FELD-2
                    WHEN X'17' MOVE '17' TO BYTE-FELD-2
                    WHEN X'18' MOVE '18' TO BYTE-FELD-2
                    WHEN X'19' MOVE '19' TO BYTE-FELD-2
                    WHEN X'1A' MOVE '1A' TO BYTE-FELD-2
                    WHEN X'1B' MOVE '1B' TO BYTE-FELD-2
                    WHEN X'1C' MOVE '1C' TO BYTE-FELD-2
                    WHEN X'1D' MOVE '1D' TO BYTE-FELD-2
                    WHEN X'1E' MOVE '1E' TO BYTE-FELD-2
                    WHEN X'1F' MOVE '1F' TO BYTE-FELD-2
                    WHEN X'20' MOVE '20' TO BYTE-FELD-2
                    WHEN X'21' MOVE '21' TO BYTE-FELD-2
                    WHEN X'22' MOVE '22' TO BYTE-FELD-2
                    WHEN X'23' MOVE '23' TO BYTE-FELD-2
                    WHEN X'24' MOVE '24' TO BYTE-FELD-2
                    WHEN X'25' MOVE '25' TO BYTE-FELD-2
                    WHEN X'26' MOVE '26' TO BYTE-FELD-2
                    WHEN X'27' MOVE '27' TO BYTE-FELD-2
                    WHEN X'28' MOVE '28' TO BYTE-FELD-2
                    WHEN X'29' MOVE '29' TO BYTE-FELD-2
                    WHEN X'2A' MOVE '2A' TO BYTE-FELD-2
                    WHEN X'2B' MOVE '2B' TO BYTE-FELD-2
                    WHEN X'2C' MOVE '2C' TO BYTE-FELD-2
                    WHEN X'2D' MOVE '2D' TO BYTE-FELD-2
                    WHEN X'2E' MOVE '2E' TO BYTE-FELD-2
                    WHEN X'2F' MOVE '2F' TO BYTE-FELD-2
                    WHEN X'30' MOVE '30' TO BYTE-FELD-2
                    WHEN X'31' MOVE '31' TO BYTE-FELD-2
                    WHEN X'32' MOVE '32' TO BYTE-FELD-2
                    WHEN X'33' MOVE '33' TO BYTE-FELD-2
                    WHEN X'34' MOVE '34' TO BYTE-FELD-2
                    WHEN X'35' MOVE '35' TO BYTE-FELD-2
                    WHEN X'36' MOVE '36' TO BYTE-FELD-2
                    WHEN X'37' MOVE '37' TO BYTE-FELD-2
                    WHEN X'38' MOVE '38' TO BYTE-FELD-2
                    WHEN X'39' MOVE '39' TO BYTE-FELD-2
                    WHEN X'3A' MOVE '3A' TO BYTE-FELD-2
                    WHEN X'3B' MOVE '3B' TO BYTE-FELD-2
                    WHEN X'3C' MOVE '3C' TO BYTE-FELD-2
                    WHEN X'3D' MOVE '3D' TO BYTE-FELD-2
                    WHEN X'3E' MOVE '3E' TO BYTE-FELD-2
                    WHEN X'3F' MOVE '3F' TO BYTE-FELD-2
                    WHEN X'40' MOVE '40' TO BYTE-FELD-2
                    WHEN X'41' MOVE '41' TO BYTE-FELD-2
                    WHEN X'42' MOVE '42' TO BYTE-FELD-2
                    WHEN X'43' MOVE '43' TO BYTE-FELD-2
                    WHEN X'44' MOVE '44' TO BYTE-FELD-2
                    WHEN X'45' MOVE '45' TO BYTE-FELD-2
                    WHEN X'46' MOVE '46' TO BYTE-FELD-2
                    WHEN X'47' MOVE '47' TO BYTE-FELD-2
                    WHEN X'48' MOVE '48' TO BYTE-FELD-2
                    WHEN X'49' MOVE '49' TO BYTE-FELD-2
                    WHEN X'4A' MOVE '4A' TO BYTE-FELD-2
                    WHEN X'4B' MOVE '4B' TO BYTE-FELD-2
                    WHEN X'4C' MOVE '4C' TO BYTE-FELD-2
                    WHEN X'4D' MOVE '4D' TO BYTE-FELD-2
                    WHEN X'4E' MOVE '4E' TO BYTE-FELD-2
                    WHEN X'4F' MOVE '4F' TO BYTE-FELD-2
                    WHEN X'50' MOVE '50' TO BYTE-FELD-2
                    WHEN X'51' MOVE '51' TO BYTE-FELD-2
                    WHEN X'52' MOVE '52' TO BYTE-FELD-2
                    WHEN X'53' MOVE '53' TO BYTE-FELD-2
                    WHEN X'54' MOVE '54' TO BYTE-FELD-2
                    WHEN X'55' MOVE '55' TO BYTE-FELD-2
                    WHEN X'56' MOVE '56' TO BYTE-FELD-2
                    WHEN X'57' MOVE '57' TO BYTE-FELD-2
                    WHEN X'58' MOVE '58' TO BYTE-FELD-2
                    WHEN X'59' MOVE '59' TO BYTE-FELD-2
                    WHEN X'5A' MOVE '5A' TO BYTE-FELD-2
                    WHEN X'5B' MOVE '5B' TO BYTE-FELD-2
                    WHEN X'5C' MOVE '5C' TO BYTE-FELD-2
                    WHEN X'5D' MOVE '5D' TO BYTE-FELD-2
                    WHEN X'5E' MOVE '5E' TO BYTE-FELD-2
                    WHEN X'5F' MOVE '5F' TO BYTE-FELD-2
                    WHEN X'60' MOVE '60' TO BYTE-FELD-2
                    WHEN X'61' MOVE '61' TO BYTE-FELD-2
                    WHEN X'62' MOVE '62' TO BYTE-FELD-2
                    WHEN X'63' MOVE '63' TO BYTE-FELD-2
                    WHEN X'64' MOVE '64' TO BYTE-FELD-2
                    WHEN X'65' MOVE '65' TO BYTE-FELD-2
                    WHEN X'66' MOVE '66' TO BYTE-FELD-2
                    WHEN X'67' MOVE '67' TO BYTE-FELD-2
                    WHEN X'68' MOVE '68' TO BYTE-FELD-2
                    WHEN X'69' MOVE '69' TO BYTE-FELD-2
                    WHEN X'6A' MOVE '6A' TO BYTE-FELD-2
                    WHEN X'6B' MOVE '6B' TO BYTE-FELD-2
                    WHEN X'6C' MOVE '6C' TO BYTE-FELD-2
                    WHEN X'6D' MOVE '6D' TO BYTE-FELD-2
                    WHEN X'6E' MOVE '6E' TO BYTE-FELD-2
                    WHEN X'6F' MOVE '6F' TO BYTE-FELD-2
                    WHEN X'70' MOVE '70' TO BYTE-FELD-2
                    WHEN X'71' MOVE '71' TO BYTE-FELD-2
                    WHEN X'72' MOVE '72' TO BYTE-FELD-2
                    WHEN X'73' MOVE '73' TO BYTE-FELD-2
                    WHEN X'74' MOVE '74' TO BYTE-FELD-2
                    WHEN X'75' MOVE '75' TO BYTE-FELD-2
                    WHEN X'76' MOVE '76' TO BYTE-FELD-2
                    WHEN X'77' MOVE '77' TO BYTE-FELD-2
                    WHEN X'78' MOVE '78' TO BYTE-FELD-2
                    WHEN X'79' MOVE '79' TO BYTE-FELD-2
                    WHEN X'7A' MOVE '7A' TO BYTE-FELD-2
                    WHEN X'7B' MOVE '7B' TO BYTE-FELD-2
                    WHEN X'7C' MOVE '7C' TO BYTE-FELD-2
                    WHEN X'7D' MOVE '7D' TO BYTE-FELD-2
                    WHEN X'7E' MOVE '7E' TO BYTE-FELD-2
                    WHEN X'7F' MOVE '7F' TO BYTE-FELD-2
                    WHEN X'80' MOVE '80' TO BYTE-FELD-2
                    WHEN X'81' MOVE '81' TO BYTE-FELD-2
                    WHEN X'82' MOVE '82' TO BYTE-FELD-2
                    WHEN X'83' MOVE '83' TO BYTE-FELD-2
                    WHEN X'84' MOVE '84' TO BYTE-FELD-2
                    WHEN X'85' MOVE '85' TO BYTE-FELD-2
                    WHEN X'86' MOVE '86' TO BYTE-FELD-2
                    WHEN X'87' MOVE '87' TO BYTE-FELD-2
                    WHEN X'88' MOVE '88' TO BYTE-FELD-2
                    WHEN X'89' MOVE '89' TO BYTE-FELD-2
                    WHEN X'8A' MOVE '8A' TO BYTE-FELD-2
                    WHEN X'8B' MOVE '8B' TO BYTE-FELD-2
                    WHEN X'8C' MOVE '8C' TO BYTE-FELD-2
                    WHEN X'8D' MOVE '8D' TO BYTE-FELD-2
                    WHEN X'8E' MOVE '8E' TO BYTE-FELD-2
                    WHEN X'8F' MOVE '8F' TO BYTE-FELD-2
                    WHEN X'90' MOVE '90' TO BYTE-FELD-2
                    WHEN X'91' MOVE '91' TO BYTE-FELD-2
                    WHEN X'92' MOVE '92' TO BYTE-FELD-2
                    WHEN X'93' MOVE '93' TO BYTE-FELD-2
                    WHEN X'94' MOVE '94' TO BYTE-FELD-2
                    WHEN X'95' MOVE '95' TO BYTE-FELD-2
                    WHEN X'96' MOVE '96' TO BYTE-FELD-2
                    WHEN X'97' MOVE '97' TO BYTE-FELD-2
                    WHEN X'98' MOVE '98' TO BYTE-FELD-2
                    WHEN X'99' MOVE '99' TO BYTE-FELD-2
                    WHEN X'9A' MOVE '9A' TO BYTE-FELD-2
                    WHEN X'9B' MOVE '9B' TO BYTE-FELD-2
                    WHEN X'9C' MOVE '9C' TO BYTE-FELD-2
                    WHEN X'9D' MOVE '9D' TO BYTE-FELD-2
                    WHEN X'9E' MOVE '9E' TO BYTE-FELD-2
                    WHEN X'9F' MOVE '9F' TO BYTE-FELD-2
                    WHEN X'A0' MOVE 'A0' TO BYTE-FELD-2
                    WHEN X'A1' MOVE 'A1' TO BYTE-FELD-2
                    WHEN X'A2' MOVE 'A2' TO BYTE-FELD-2
                    WHEN X'A3' MOVE 'A3' TO BYTE-FELD-2
                    WHEN X'A4' MOVE 'A4' TO BYTE-FELD-2
                    WHEN X'A5' MOVE 'A5' TO BYTE-FELD-2
                    WHEN X'A6' MOVE 'A6' TO BYTE-FELD-2
                    WHEN X'A7' MOVE 'A7' TO BYTE-FELD-2
                    WHEN X'A8' MOVE 'A8' TO BYTE-FELD-2
                    WHEN X'A9' MOVE 'A9' TO BYTE-FELD-2
                    WHEN X'AA' MOVE 'AA' TO BYTE-FELD-2
                    WHEN X'AB' MOVE 'AB' TO BYTE-FELD-2
                    WHEN X'AC' MOVE 'AC' TO BYTE-FELD-2
                    WHEN X'AD' MOVE 'AD' TO BYTE-FELD-2
                    WHEN X'AE' MOVE 'AE' TO BYTE-FELD-2
                    WHEN X'AF' MOVE 'AF' TO BYTE-FELD-2
                    WHEN X'B0' MOVE 'B0' TO BYTE-FELD-2
                    WHEN X'B1' MOVE 'B1' TO BYTE-FELD-2
                    WHEN X'B2' MOVE 'B2' TO BYTE-FELD-2
                    WHEN X'B3' MOVE 'B3' TO BYTE-FELD-2
                    WHEN X'B4' MOVE 'B4' TO BYTE-FELD-2
                    WHEN X'B5' MOVE 'B5' TO BYTE-FELD-2
                    WHEN X'B6' MOVE 'B6' TO BYTE-FELD-2
                    WHEN X'B7' MOVE 'B7' TO BYTE-FELD-2
                    WHEN X'B8' MOVE 'B8' TO BYTE-FELD-2
                    WHEN X'B9' MOVE 'B9' TO BYTE-FELD-2
                    WHEN X'BA' MOVE 'BA' TO BYTE-FELD-2
                    WHEN X'BB' MOVE 'BB' TO BYTE-FELD-2
                    WHEN X'BC' MOVE 'BC' TO BYTE-FELD-2
                    WHEN X'BD' MOVE 'BD' TO BYTE-FELD-2
                    WHEN X'BE' MOVE 'BE' TO BYTE-FELD-2
                    WHEN X'BF' MOVE 'BF' TO BYTE-FELD-2
                    WHEN X'C0' MOVE 'C0' TO BYTE-FELD-2
                    WHEN X'C1' MOVE 'C1' TO BYTE-FELD-2
                    WHEN X'C2' MOVE 'C2' TO BYTE-FELD-2
                    WHEN X'C3' MOVE 'C3' TO BYTE-FELD-2
                    WHEN X'C4' MOVE 'C4' TO BYTE-FELD-2
                    WHEN X'C5' MOVE 'C5' TO BYTE-FELD-2
                    WHEN X'C6' MOVE 'C6' TO BYTE-FELD-2
                    WHEN X'C7' MOVE 'C7' TO BYTE-FELD-2
                    WHEN X'C8' MOVE 'C8' TO BYTE-FELD-2
                    WHEN X'C9' MOVE 'C9' TO BYTE-FELD-2
                    WHEN X'CA' MOVE 'CA' TO BYTE-FELD-2
                    WHEN X'CB' MOVE 'CB' TO BYTE-FELD-2
                    WHEN X'CC' MOVE 'CC' TO BYTE-FELD-2
                    WHEN X'CD' MOVE 'CD' TO BYTE-FELD-2
                    WHEN X'CE' MOVE 'CE' TO BYTE-FELD-2
                    WHEN X'CF' MOVE 'CF' TO BYTE-FELD-2
                    WHEN X'D0' MOVE 'D0' TO BYTE-FELD-2
                    WHEN X'D1' MOVE 'D1' TO BYTE-FELD-2
                    WHEN X'D2' MOVE 'D2' TO BYTE-FELD-2
                    WHEN X'D3' MOVE 'D3' TO BYTE-FELD-2
                    WHEN X'D4' MOVE 'D4' TO BYTE-FELD-2
                    WHEN X'D5' MOVE 'D5' TO BYTE-FELD-2
                    WHEN X'D6' MOVE 'D6' TO BYTE-FELD-2
                    WHEN X'D7' MOVE 'D7' TO BYTE-FELD-2
                    WHEN X'D8' MOVE 'D8' TO BYTE-FELD-2
                    WHEN X'D9' MOVE 'D9' TO BYTE-FELD-2
                    WHEN X'DA' MOVE 'DA' TO BYTE-FELD-2
                    WHEN X'DB' MOVE 'DB' TO BYTE-FELD-2
                    WHEN X'DC' MOVE 'DC' TO BYTE-FELD-2
                    WHEN X'DD' MOVE 'DD' TO BYTE-FELD-2
                    WHEN X'DE' MOVE 'DE' TO BYTE-FELD-2
                    WHEN X'DF' MOVE 'DF' TO BYTE-FELD-2
                    WHEN X'E0' MOVE 'E0' TO BYTE-FELD-2
                    WHEN X'E1' MOVE 'E1' TO BYTE-FELD-2
                    WHEN X'E2' MOVE 'E2' TO BYTE-FELD-2
                    WHEN X'E3' MOVE 'E3' TO BYTE-FELD-2
                    WHEN X'E4' MOVE 'E4' TO BYTE-FELD-2
                    WHEN X'E5' MOVE 'E5' TO BYTE-FELD-2
                    WHEN X'E6' MOVE 'E6' TO BYTE-FELD-2
                    WHEN X'E7' MOVE 'E7' TO BYTE-FELD-2
                    WHEN X'E8' MOVE 'E8' TO BYTE-FELD-2
                    WHEN X'E9' MOVE 'E9' TO BYTE-FELD-2
                    WHEN X'EA' MOVE 'EA' TO BYTE-FELD-2
                    WHEN X'EB' MOVE 'EB' TO BYTE-FELD-2
                    WHEN X'EC' MOVE 'EC' TO BYTE-FELD-2
                    WHEN X'ED' MOVE 'ED' TO BYTE-FELD-2
                    WHEN X'EE' MOVE 'EE' TO BYTE-FELD-2
                    WHEN X'EF' MOVE 'EF' TO BYTE-FELD-2
                    WHEN X'F0' MOVE 'F0' TO BYTE-FELD-2
                    WHEN X'F1' MOVE 'F1' TO BYTE-FELD-2
                    WHEN X'F2' MOVE 'F2' TO BYTE-FELD-2
                    WHEN X'F3' MOVE 'F3' TO BYTE-FELD-2
                    WHEN X'F4' MOVE 'F4' TO BYTE-FELD-2
                    WHEN X'F5' MOVE 'F5' TO BYTE-FELD-2
                    WHEN X'F6' MOVE 'F6' TO BYTE-FELD-2
                    WHEN X'F7' MOVE 'F7' TO BYTE-FELD-2
                    WHEN X'F8' MOVE 'F8' TO BYTE-FELD-2
                    WHEN X'F9' MOVE 'F9' TO BYTE-FELD-2
                    WHEN X'FA' MOVE 'FA' TO BYTE-FELD-2
                    WHEN X'FB' MOVE 'FB' TO BYTE-FELD-2
                    WHEN X'FC' MOVE 'FC' TO BYTE-FELD-2
                    WHEN X'FD' MOVE 'FD' TO BYTE-FELD-2
                    WHEN X'FE' MOVE 'FE' TO BYTE-FELD-2
                    WHEN X'FF' MOVE 'FF' TO BYTE-FELD-2
             END-EVALUATE

             COMPUTE HEX-Y = (HEX-I - 1) * 2 + 1

             MOVE BYTE-FELD-2 TO BYTE-FELD-26 (HEX-Y:2)

           END-PERFORM
