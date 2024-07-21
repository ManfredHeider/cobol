      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   UU015S.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\UU015S.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 15:15:06   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.0   Nov 04 1995 09:45:56   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *****  VERSION   1    30.06.93 16:46:08
      *************************************************
      **   VB-DIALOG,                                **
      **   BETREUUNGSTABELLE                         **
      **---------------------------------------------**
      **   AENDERUNG - 07.10.86 - WIECHERS           **
      **   FOLGENDE TABELLENELEMENTE WURDEN IN DIE   **
      **   TABELLE AUFGENOMMEN:                      **
      **   - 757858 (EDA/LD)                         **
      **   - 768858 (ED/LD)                          **
      **                                             **
      **   AENDERUNG - 28.01.88 - DOBSLAFF           **
      **   FOLGENDE TABELLENELEMENTE WURDEN IN DIE   **
      **   TABELLE AUFGENOMMEN:                      **
      **   - 016016 (VBA1/VBA1)                      **
      **   - 016018 (VBA1/VBA2)                      **
      **   - 016019 (VBA1/VBA3)                      **
      **   - 018016 (VBA2/VBA1)                      **
      **   - 018018 (VBA2/VBA2)                      **
      **   - 018019 (VBA2/VBA3)                      **
      **   - 019016 (VBA3/VBA1)                      **
      **   - 019018 (VBA3/VBA2)                      **
      **   - 019019 (VBA3/VBA3)                      **
      **                                             **
      **   AENDERUNG - 10.03.88 - DOBSLAFF           **
      **   MIT DER EINFUEHRUNG REGIONALGESCHAEFTS-   **
      **   STELLENLEITER AUF OCC. 511 ERWEITERT.     **
      **                                             **
      **   AENDERUNG - 19.03.91 - NAUMANN            **
      **   MIT DER EINFUEHRUNG RANGFREI, D.H.:       **
      **   STUFE RF-T M. STUF-NR. = 888,             **
      **   STUFE RF-R M. STUF-NR. = 889,             **
      **   STUFE RF-A M. STUF-NR. = 890              **
      **   VON OCC. 511 AUF OCC. 697 ERWEITERT.      **
      **                                             **
      **   AENDERUNG - 30.06.93 - HECKER             **
      **   D KANN D BETREUEN                         **
      **   VON OCC. 697 AUF OCC. 698 ERWEITERT.      **
      *************************************************
      *
       01  T2-BETREUUNGS-TAB.
           02  FILLER                    PIC 9(6) VALUE 001010.
           02  FILLER                    PIC 9(6) VALUE 001011.
           02  FILLER                    PIC 9(6) VALUE 001016.
           02  FILLER                    PIC 9(6) VALUE 001018.
           02  FILLER                    PIC 9(6) VALUE 001019.
           02  FILLER                    PIC 9(6) VALUE 001121.
           02  FILLER                    PIC 9(6) VALUE 001222.
           02  FILLER                    PIC 9(6) VALUE 001323.
           02  FILLER                    PIC 9(6) VALUE 001424.
           02  FILLER                    PIC 9(6) VALUE 001525.
           02  FILLER                    PIC 9(6) VALUE 001626.
           02  FILLER                    PIC 9(6) VALUE 001252.
           02  FILLER                    PIC 9(6) VALUE 001161.
           02  FILLER                    PIC 9(6) VALUE 001262.
           02  FILLER                    PIC 9(6) VALUE 001272.
           02  FILLER                    PIC 9(6) VALUE 001353.
           02  FILLER                    PIC 9(6) VALUE 001454.
           02  FILLER                    PIC 9(6) VALUE 001465.
           02  FILLER                    PIC 9(6) VALUE 001555.
           02  FILLER                    PIC 9(6) VALUE 001656.
           02  FILLER                    PIC 9(6) VALUE 001757.
           02  FILLER                    PIC 9(6) VALUE 001858.
           02  FILLER                    PIC 9(6) VALUE 001363.
           02  FILLER                    PIC 9(6) VALUE 001464.
           02  FILLER                    PIC 9(6) VALUE 001566.
           02  FILLER                    PIC 9(6) VALUE 001667.
           02  FILLER                    PIC 9(6) VALUE 001768.
           02  FILLER                    PIC 9(6) VALUE 001800.
           02  FILLER                    PIC 9(6) VALUE 001888.
           02  FILLER                    PIC 9(6) VALUE 001889.
           02  FILLER                    PIC 9(6) VALUE 001890.
      *
           02  FILLER                    PIC 9(6) VALUE 002010.
           02  FILLER                    PIC 9(6) VALUE 002011.
           02  FILLER                    PIC 9(6) VALUE 002016.
           02  FILLER                    PIC 9(6) VALUE 002018.
           02  FILLER                    PIC 9(6) VALUE 002019.
           02  FILLER                    PIC 9(6) VALUE 002121.
           02  FILLER                    PIC 9(6) VALUE 002222.
           02  FILLER                    PIC 9(6) VALUE 002323.
           02  FILLER                    PIC 9(6) VALUE 002424.
           02  FILLER                    PIC 9(6) VALUE 002525.
           02  FILLER                    PIC 9(6) VALUE 002626.
           02  FILLER                    PIC 9(6) VALUE 002252.
           02  FILLER                    PIC 9(6) VALUE 002161.
           02  FILLER                    PIC 9(6) VALUE 002262.
           02  FILLER                    PIC 9(6) VALUE 002272.
           02  FILLER                    PIC 9(6) VALUE 002353.
           02  FILLER                    PIC 9(6) VALUE 002454.
           02  FILLER                    PIC 9(6) VALUE 002465.
           02  FILLER                    PIC 9(6) VALUE 002555.
           02  FILLER                    PIC 9(6) VALUE 002656.
           02  FILLER                    PIC 9(6) VALUE 002757.
           02  FILLER                    PIC 9(6) VALUE 002858.
           02  FILLER                    PIC 9(6) VALUE 002363.
           02  FILLER                    PIC 9(6) VALUE 002464.
           02  FILLER                    PIC 9(6) VALUE 002566.
           02  FILLER                    PIC 9(6) VALUE 002667.
           02  FILLER                    PIC 9(6) VALUE 002768.
           02  FILLER                    PIC 9(6) VALUE 002800.
           02  FILLER                    PIC 9(6) VALUE 002888.
           02  FILLER                    PIC 9(6) VALUE 002889.
           02  FILLER                    PIC 9(6) VALUE 002890.
      *
           02  FILLER                    PIC 9(6) VALUE 003010.
           02  FILLER                    PIC 9(6) VALUE 003011.
           02  FILLER                    PIC 9(6) VALUE 003016.
           02  FILLER                    PIC 9(6) VALUE 003018.
           02  FILLER                    PIC 9(6) VALUE 003019.
           02  FILLER                    PIC 9(6) VALUE 003121.
           02  FILLER                    PIC 9(6) VALUE 003222.
           02  FILLER                    PIC 9(6) VALUE 003323.
           02  FILLER                    PIC 9(6) VALUE 003424.
           02  FILLER                    PIC 9(6) VALUE 003525.
           02  FILLER                    PIC 9(6) VALUE 003626.
           02  FILLER                    PIC 9(6) VALUE 003252.
           02  FILLER                    PIC 9(6) VALUE 003161.
           02  FILLER                    PIC 9(6) VALUE 003262.
           02  FILLER                    PIC 9(6) VALUE 003272.
           02  FILLER                    PIC 9(6) VALUE 003353.
           02  FILLER                    PIC 9(6) VALUE 003454.
           02  FILLER                    PIC 9(6) VALUE 003465.
           02  FILLER                    PIC 9(6) VALUE 003555.
           02  FILLER                    PIC 9(6) VALUE 003656.
           02  FILLER                    PIC 9(6) VALUE 003757.
           02  FILLER                    PIC 9(6) VALUE 003858.
           02  FILLER                    PIC 9(6) VALUE 003363.
           02  FILLER                    PIC 9(6) VALUE 003464.
           02  FILLER                    PIC 9(6) VALUE 003566.
           02  FILLER                    PIC 9(6) VALUE 003667.
           02  FILLER                    PIC 9(6) VALUE 003768.
           02  FILLER                    PIC 9(6) VALUE 003800.
           02  FILLER                    PIC 9(6) VALUE 003888.
           02  FILLER                    PIC 9(6) VALUE 003889.
           02  FILLER                    PIC 9(6) VALUE 003890.
      *
           02  FILLER                    PIC 9(6) VALUE 004010.
           02  FILLER                    PIC 9(6) VALUE 004011.
           02  FILLER                    PIC 9(6) VALUE 004016.
           02  FILLER                    PIC 9(6) VALUE 004018.
           02  FILLER                    PIC 9(6) VALUE 004019.
           02  FILLER                    PIC 9(6) VALUE 004121.
           02  FILLER                    PIC 9(6) VALUE 004222.
           02  FILLER                    PIC 9(6) VALUE 004323.
           02  FILLER                    PIC 9(6) VALUE 004424.
           02  FILLER                    PIC 9(6) VALUE 004525.
           02  FILLER                    PIC 9(6) VALUE 004626.
           02  FILLER                    PIC 9(6) VALUE 004252.
           02  FILLER                    PIC 9(6) VALUE 004161.
           02  FILLER                    PIC 9(6) VALUE 004262.
           02  FILLER                    PIC 9(6) VALUE 004272.
           02  FILLER                    PIC 9(6) VALUE 004353.
           02  FILLER                    PIC 9(6) VALUE 004454.
           02  FILLER                    PIC 9(6) VALUE 004465.
           02  FILLER                    PIC 9(6) VALUE 004555.
           02  FILLER                    PIC 9(6) VALUE 004656.
           02  FILLER                    PIC 9(6) VALUE 004757.
           02  FILLER                    PIC 9(6) VALUE 004858.
           02  FILLER                    PIC 9(6) VALUE 004363.
           02  FILLER                    PIC 9(6) VALUE 004464.
           02  FILLER                    PIC 9(6) VALUE 004566.
           02  FILLER                    PIC 9(6) VALUE 004667.
           02  FILLER                    PIC 9(6) VALUE 004768.
           02  FILLER                    PIC 9(6) VALUE 004800.
           02  FILLER                    PIC 9(6) VALUE 004888.
           02  FILLER                    PIC 9(6) VALUE 004889.
           02  FILLER                    PIC 9(6) VALUE 004890.
      *
           02  FILLER                    PIC 9(6) VALUE 009010.
           02  FILLER                    PIC 9(6) VALUE 009011.
           02  FILLER                    PIC 9(6) VALUE 009016.
           02  FILLER                    PIC 9(6) VALUE 009018.
           02  FILLER                    PIC 9(6) VALUE 009019.
           02  FILLER                    PIC 9(6) VALUE 009121.
           02  FILLER                    PIC 9(6) VALUE 009222.
           02  FILLER                    PIC 9(6) VALUE 009323.
           02  FILLER                    PIC 9(6) VALUE 009424.
           02  FILLER                    PIC 9(6) VALUE 009525.
           02  FILLER                    PIC 9(6) VALUE 009626.
           02  FILLER                    PIC 9(6) VALUE 009252.
           02  FILLER                    PIC 9(6) VALUE 009161.
           02  FILLER                    PIC 9(6) VALUE 009262.
           02  FILLER                    PIC 9(6) VALUE 009272.
           02  FILLER                    PIC 9(6) VALUE 009353.
           02  FILLER                    PIC 9(6) VALUE 009454.
           02  FILLER                    PIC 9(6) VALUE 009465.
           02  FILLER                    PIC 9(6) VALUE 009555.
           02  FILLER                    PIC 9(6) VALUE 009656.
           02  FILLER                    PIC 9(6) VALUE 009757.
           02  FILLER                    PIC 9(6) VALUE 009858.
           02  FILLER                    PIC 9(6) VALUE 009363.
           02  FILLER                    PIC 9(6) VALUE 009464.
           02  FILLER                    PIC 9(6) VALUE 009566.
           02  FILLER                    PIC 9(6) VALUE 009667.
           02  FILLER                    PIC 9(6) VALUE 009768.
           02  FILLER                    PIC 9(6) VALUE 009800.
           02  FILLER                    PIC 9(6) VALUE 009888.
           02  FILLER                    PIC 9(6) VALUE 009889.
           02  FILLER                    PIC 9(6) VALUE 009890.
      *
           02  FILLER                    PIC 9(6) VALUE 010121.
           02  FILLER                    PIC 9(6) VALUE 010222.
           02  FILLER                    PIC 9(6) VALUE 010323.
           02  FILLER                    PIC 9(6) VALUE 010424.
           02  FILLER                    PIC 9(6) VALUE 010525.
           02  FILLER                    PIC 9(6) VALUE 010626.
           02  FILLER                    PIC 9(6) VALUE 010252.
           02  FILLER                    PIC 9(6) VALUE 010161.
           02  FILLER                    PIC 9(6) VALUE 010262.
           02  FILLER                    PIC 9(6) VALUE 010272.
           02  FILLER                    PIC 9(6) VALUE 010353.
           02  FILLER                    PIC 9(6) VALUE 010454.
           02  FILLER                    PIC 9(6) VALUE 010465.
           02  FILLER                    PIC 9(6) VALUE 010555.
           02  FILLER                    PIC 9(6) VALUE 010656.
           02  FILLER                    PIC 9(6) VALUE 010757.
           02  FILLER                    PIC 9(6) VALUE 010858.
           02  FILLER                    PIC 9(6) VALUE 010363.
           02  FILLER                    PIC 9(6) VALUE 010464.
           02  FILLER                    PIC 9(6) VALUE 010566.
           02  FILLER                    PIC 9(6) VALUE 010667.
           02  FILLER                    PIC 9(6) VALUE 010768.
           02  FILLER                    PIC 9(6) VALUE 010800.
           02  FILLER                    PIC 9(6) VALUE 010888.
           02  FILLER                    PIC 9(6) VALUE 010889.
           02  FILLER                    PIC 9(6) VALUE 010890.
      *
           02  FILLER                    PIC 9(6) VALUE 011121.
           02  FILLER                    PIC 9(6) VALUE 011222.
           02  FILLER                    PIC 9(6) VALUE 011323.
           02  FILLER                    PIC 9(6) VALUE 011424.
           02  FILLER                    PIC 9(6) VALUE 011525.
           02  FILLER                    PIC 9(6) VALUE 011626.
           02  FILLER                    PIC 9(6) VALUE 011252.
           02  FILLER                    PIC 9(6) VALUE 011161.
           02  FILLER                    PIC 9(6) VALUE 011262.
           02  FILLER                    PIC 9(6) VALUE 011272.
           02  FILLER                    PIC 9(6) VALUE 011353.
           02  FILLER                    PIC 9(6) VALUE 011454.
           02  FILLER                    PIC 9(6) VALUE 011465.
           02  FILLER                    PIC 9(6) VALUE 011555.
           02  FILLER                    PIC 9(6) VALUE 011656.
           02  FILLER                    PIC 9(6) VALUE 011757.
           02  FILLER                    PIC 9(6) VALUE 011858.
           02  FILLER                    PIC 9(6) VALUE 011363.
           02  FILLER                    PIC 9(6) VALUE 011464.
           02  FILLER                    PIC 9(6) VALUE 011566.
           02  FILLER                    PIC 9(6) VALUE 011667.
           02  FILLER                    PIC 9(6) VALUE 011768.
           02  FILLER                    PIC 9(6) VALUE 011800.
           02  FILLER                    PIC 9(6) VALUE 011888.
           02  FILLER                    PIC 9(6) VALUE 011889.
           02  FILLER                    PIC 9(6) VALUE 011890.
      *
           02  FILLER                    PIC 9(6) VALUE 016016.
           02  FILLER                    PIC 9(6) VALUE 016018.
           02  FILLER                    PIC 9(6) VALUE 016019.
           02  FILLER                    PIC 9(6) VALUE 016121.
           02  FILLER                    PIC 9(6) VALUE 016222.
           02  FILLER                    PIC 9(6) VALUE 016323.
           02  FILLER                    PIC 9(6) VALUE 016424.
           02  FILLER                    PIC 9(6) VALUE 016525.
           02  FILLER                    PIC 9(6) VALUE 016626.
           02  FILLER                    PIC 9(6) VALUE 016252.
           02  FILLER                    PIC 9(6) VALUE 016161.
           02  FILLER                    PIC 9(6) VALUE 016262.
           02  FILLER                    PIC 9(6) VALUE 016272.
           02  FILLER                    PIC 9(6) VALUE 016353.
           02  FILLER                    PIC 9(6) VALUE 016454.
           02  FILLER                    PIC 9(6) VALUE 016465.
           02  FILLER                    PIC 9(6) VALUE 016555.
           02  FILLER                    PIC 9(6) VALUE 016656.
           02  FILLER                    PIC 9(6) VALUE 016757.
           02  FILLER                    PIC 9(6) VALUE 016858.
           02  FILLER                    PIC 9(6) VALUE 016363.
           02  FILLER                    PIC 9(6) VALUE 016464.
           02  FILLER                    PIC 9(6) VALUE 016566.
           02  FILLER                    PIC 9(6) VALUE 016667.
           02  FILLER                    PIC 9(6) VALUE 016768.
           02  FILLER                    PIC 9(6) VALUE 016800.
           02  FILLER                    PIC 9(6) VALUE 016888.
           02  FILLER                    PIC 9(6) VALUE 016889.
           02  FILLER                    PIC 9(6) VALUE 016890.
      *
           02  FILLER                    PIC 9(6) VALUE 018016.
           02  FILLER                    PIC 9(6) VALUE 018018.
           02  FILLER                    PIC 9(6) VALUE 018019.
           02  FILLER                    PIC 9(6) VALUE 018121.
           02  FILLER                    PIC 9(6) VALUE 018222.
           02  FILLER                    PIC 9(6) VALUE 018323.
           02  FILLER                    PIC 9(6) VALUE 018424.
           02  FILLER                    PIC 9(6) VALUE 018525.
           02  FILLER                    PIC 9(6) VALUE 018626.
           02  FILLER                    PIC 9(6) VALUE 018252.
           02  FILLER                    PIC 9(6) VALUE 018161.
           02  FILLER                    PIC 9(6) VALUE 018262.
           02  FILLER                    PIC 9(6) VALUE 018272.
           02  FILLER                    PIC 9(6) VALUE 018353.
           02  FILLER                    PIC 9(6) VALUE 018454.
           02  FILLER                    PIC 9(6) VALUE 018465.
           02  FILLER                    PIC 9(6) VALUE 018555.
           02  FILLER                    PIC 9(6) VALUE 018656.
           02  FILLER                    PIC 9(6) VALUE 018757.
           02  FILLER                    PIC 9(6) VALUE 018858.
           02  FILLER                    PIC 9(6) VALUE 018363.
           02  FILLER                    PIC 9(6) VALUE 018464.
           02  FILLER                    PIC 9(6) VALUE 018566.
           02  FILLER                    PIC 9(6) VALUE 018667.
           02  FILLER                    PIC 9(6) VALUE 018768.
           02  FILLER                    PIC 9(6) VALUE 018800.
           02  FILLER                    PIC 9(6) VALUE 018888.
           02  FILLER                    PIC 9(6) VALUE 018889.
           02  FILLER                    PIC 9(6) VALUE 018890.
      *
           02  FILLER                    PIC 9(6) VALUE 019016.
           02  FILLER                    PIC 9(6) VALUE 019018.
           02  FILLER                    PIC 9(6) VALUE 019019.
           02  FILLER                    PIC 9(6) VALUE 019222.
           02  FILLER                    PIC 9(6) VALUE 019323.
           02  FILLER                    PIC 9(6) VALUE 019424.
           02  FILLER                    PIC 9(6) VALUE 019525.
           02  FILLER                    PIC 9(6) VALUE 019626.
           02  FILLER                    PIC 9(6) VALUE 019252.
           02  FILLER                    PIC 9(6) VALUE 019161.
           02  FILLER                    PIC 9(6) VALUE 019262.
           02  FILLER                    PIC 9(6) VALUE 019272.
           02  FILLER                    PIC 9(6) VALUE 019353.
           02  FILLER                    PIC 9(6) VALUE 019454.
           02  FILLER                    PIC 9(6) VALUE 019465.
           02  FILLER                    PIC 9(6) VALUE 019555.
           02  FILLER                    PIC 9(6) VALUE 019656.
           02  FILLER                    PIC 9(6) VALUE 019757.
           02  FILLER                    PIC 9(6) VALUE 019858.
           02  FILLER                    PIC 9(6) VALUE 019363.
           02  FILLER                    PIC 9(6) VALUE 019464.
           02  FILLER                    PIC 9(6) VALUE 019566.
           02  FILLER                    PIC 9(6) VALUE 019667.
           02  FILLER                    PIC 9(6) VALUE 019768.
           02  FILLER                    PIC 9(6) VALUE 019800.
           02  FILLER                    PIC 9(6) VALUE 019888.
           02  FILLER                    PIC 9(6) VALUE 019889.
           02  FILLER                    PIC 9(6) VALUE 019890.
      *
           02  FILLER                    PIC 9(6) VALUE 121222.
           02  FILLER                    PIC 9(6) VALUE 121323.
           02  FILLER                    PIC 9(6) VALUE 121424.
           02  FILLER                    PIC 9(6) VALUE 121525.
           02  FILLER                    PIC 9(6) VALUE 121626.
           02  FILLER                    PIC 9(6) VALUE 121252.
           02  FILLER                    PIC 9(6) VALUE 121161.
           02  FILLER                    PIC 9(6) VALUE 121262.
           02  FILLER                    PIC 9(6) VALUE 121272.
           02  FILLER                    PIC 9(6) VALUE 121353.
           02  FILLER                    PIC 9(6) VALUE 121454.
           02  FILLER                    PIC 9(6) VALUE 121465.
           02  FILLER                    PIC 9(6) VALUE 121555.
           02  FILLER                    PIC 9(6) VALUE 121656.
           02  FILLER                    PIC 9(6) VALUE 121757.
           02  FILLER                    PIC 9(6) VALUE 121858.
           02  FILLER                    PIC 9(6) VALUE 121363.
           02  FILLER                    PIC 9(6) VALUE 121464.
           02  FILLER                    PIC 9(6) VALUE 121566.
           02  FILLER                    PIC 9(6) VALUE 121667.
           02  FILLER                    PIC 9(6) VALUE 121768.
           02  FILLER                    PIC 9(6) VALUE 121800.
           02  FILLER                    PIC 9(6) VALUE 121888.
           02  FILLER                    PIC 9(6) VALUE 121889.
           02  FILLER                    PIC 9(6) VALUE 121890.
      *
           02  FILLER                    PIC 9(6) VALUE 161222.
           02  FILLER                    PIC 9(6) VALUE 161323.
           02  FILLER                    PIC 9(6) VALUE 161424.
           02  FILLER                    PIC 9(6) VALUE 161525.
           02  FILLER                    PIC 9(6) VALUE 161626.
           02  FILLER                    PIC 9(6) VALUE 161252.
           02  FILLER                    PIC 9(6) VALUE 161161.
           02  FILLER                    PIC 9(6) VALUE 161262.
           02  FILLER                    PIC 9(6) VALUE 161272.
           02  FILLER                    PIC 9(6) VALUE 161353.
           02  FILLER                    PIC 9(6) VALUE 161454.
           02  FILLER                    PIC 9(6) VALUE 161465.
           02  FILLER                    PIC 9(6) VALUE 161555.
           02  FILLER                    PIC 9(6) VALUE 161656.
           02  FILLER                    PIC 9(6) VALUE 161757.
           02  FILLER                    PIC 9(6) VALUE 161858.
           02  FILLER                    PIC 9(6) VALUE 161363.
           02  FILLER                    PIC 9(6) VALUE 161464.
           02  FILLER                    PIC 9(6) VALUE 161566.
           02  FILLER                    PIC 9(6) VALUE 161667.
           02  FILLER                    PIC 9(6) VALUE 161768.
           02  FILLER                    PIC 9(6) VALUE 161800.
           02  FILLER                    PIC 9(6) VALUE 161888.
           02  FILLER                    PIC 9(6) VALUE 161889.
           02  FILLER                    PIC 9(6) VALUE 161890.
      *
           02  FILLER                    PIC 9(6) VALUE 222323.
           02  FILLER                    PIC 9(6) VALUE 222424.
           02  FILLER                    PIC 9(6) VALUE 222525.
           02  FILLER                    PIC 9(6) VALUE 222626.
           02  FILLER                    PIC 9(6) VALUE 222252.
           02  FILLER                    PIC 9(6) VALUE 222262.
           02  FILLER                    PIC 9(6) VALUE 222272.
           02  FILLER                    PIC 9(6) VALUE 222353.
           02  FILLER                    PIC 9(6) VALUE 222454.
           02  FILLER                    PIC 9(6) VALUE 222465.
           02  FILLER                    PIC 9(6) VALUE 222555.
           02  FILLER                    PIC 9(6) VALUE 222656.
           02  FILLER                    PIC 9(6) VALUE 222757.
           02  FILLER                    PIC 9(6) VALUE 222858.
           02  FILLER                    PIC 9(6) VALUE 222363.
           02  FILLER                    PIC 9(6) VALUE 222464.
           02  FILLER                    PIC 9(6) VALUE 222566.
           02  FILLER                    PIC 9(6) VALUE 222667.
           02  FILLER                    PIC 9(6) VALUE 222768.
           02  FILLER                    PIC 9(6) VALUE 222800.
           02  FILLER                    PIC 9(6) VALUE 222888.
           02  FILLER                    PIC 9(6) VALUE 222889.
           02  FILLER                    PIC 9(6) VALUE 222890.
      *
           02  FILLER                    PIC 9(6) VALUE 252252.
           02  FILLER                    PIC 9(6) VALUE 252262.
           02  FILLER                    PIC 9(6) VALUE 252272.
           02  FILLER                    PIC 9(6) VALUE 252353.
           02  FILLER                    PIC 9(6) VALUE 252363.
           02  FILLER                    PIC 9(6) VALUE 252454.
           02  FILLER                    PIC 9(6) VALUE 252464.
           02  FILLER                    PIC 9(6) VALUE 252465.
           02  FILLER                    PIC 9(6) VALUE 252555.
           02  FILLER                    PIC 9(6) VALUE 252566.
           02  FILLER                    PIC 9(6) VALUE 252667.
           02  FILLER                    PIC 9(6) VALUE 252656.
           02  FILLER                    PIC 9(6) VALUE 252757.
           02  FILLER                    PIC 9(6) VALUE 252768.
           02  FILLER                    PIC 9(6) VALUE 252858.
           02  FILLER                    PIC 9(6) VALUE 252800.
           02  FILLER                    PIC 9(6) VALUE 252888.
           02  FILLER                    PIC 9(6) VALUE 252889.
           02  FILLER                    PIC 9(6) VALUE 252890.
      *
           02  FILLER                    PIC 9(6) VALUE 262252.
           02  FILLER                    PIC 9(6) VALUE 262262.
           02  FILLER                    PIC 9(6) VALUE 262272.
           02  FILLER                    PIC 9(6) VALUE 262353.
           02  FILLER                    PIC 9(6) VALUE 262363.
           02  FILLER                    PIC 9(6) VALUE 262454.
           02  FILLER                    PIC 9(6) VALUE 262464.
           02  FILLER                    PIC 9(6) VALUE 262465.
           02  FILLER                    PIC 9(6) VALUE 262555.
           02  FILLER                    PIC 9(6) VALUE 262566.
           02  FILLER                    PIC 9(6) VALUE 262667.
           02  FILLER                    PIC 9(6) VALUE 262656.
           02  FILLER                    PIC 9(6) VALUE 262757.
           02  FILLER                    PIC 9(6) VALUE 262768.
           02  FILLER                    PIC 9(6) VALUE 262858.
           02  FILLER                    PIC 9(6) VALUE 262800.
           02  FILLER                    PIC 9(6) VALUE 262888.
           02  FILLER                    PIC 9(6) VALUE 262889.
           02  FILLER                    PIC 9(6) VALUE 262890.
      *
           02  FILLER                    PIC 9(6) VALUE 272252.
           02  FILLER                    PIC 9(6) VALUE 272262.
           02  FILLER                    PIC 9(6) VALUE 272272.
           02  FILLER                    PIC 9(6) VALUE 272353.
           02  FILLER                    PIC 9(6) VALUE 272363.
           02  FILLER                    PIC 9(6) VALUE 272454.
           02  FILLER                    PIC 9(6) VALUE 272464.
           02  FILLER                    PIC 9(6) VALUE 272465.
           02  FILLER                    PIC 9(6) VALUE 272555.
           02  FILLER                    PIC 9(6) VALUE 272566.
           02  FILLER                    PIC 9(6) VALUE 272667.
           02  FILLER                    PIC 9(6) VALUE 272656.
           02  FILLER                    PIC 9(6) VALUE 272757.
           02  FILLER                    PIC 9(6) VALUE 272768.
           02  FILLER                    PIC 9(6) VALUE 272858.
           02  FILLER                    PIC 9(6) VALUE 272800.
           02  FILLER                    PIC 9(6) VALUE 272888.
           02  FILLER                    PIC 9(6) VALUE 272889.
           02  FILLER                    PIC 9(6) VALUE 272890.
      *
           02  FILLER                    PIC 9(6) VALUE 323424.
           02  FILLER                    PIC 9(6) VALUE 323525.
           02  FILLER                    PIC 9(6) VALUE 323626.
           02  FILLER                    PIC 9(6) VALUE 323353.
           02  FILLER                    PIC 9(6) VALUE 323454.
           02  FILLER                    PIC 9(6) VALUE 323555.
           02  FILLER                    PIC 9(6) VALUE 323656.
           02  FILLER                    PIC 9(6) VALUE 323757.
           02  FILLER                    PIC 9(6) VALUE 323858.
           02  FILLER                    PIC 9(6) VALUE 323363.
           02  FILLER                    PIC 9(6) VALUE 323464.
           02  FILLER                    PIC 9(6) VALUE 323465.
           02  FILLER                    PIC 9(6) VALUE 323566.
           02  FILLER                    PIC 9(6) VALUE 323667.
           02  FILLER                    PIC 9(6) VALUE 323768.
           02  FILLER                    PIC 9(6) VALUE 323800.
           02  FILLER                    PIC 9(6) VALUE 323888.
           02  FILLER                    PIC 9(6) VALUE 323889.
           02  FILLER                    PIC 9(6) VALUE 323890.
      *
           02  FILLER                    PIC 9(6) VALUE 353353.
           02  FILLER                    PIC 9(6) VALUE 353454.
           02  FILLER                    PIC 9(6) VALUE 353555.
           02  FILLER                    PIC 9(6) VALUE 353656.
           02  FILLER                    PIC 9(6) VALUE 353757.
           02  FILLER                    PIC 9(6) VALUE 353858.
           02  FILLER                    PIC 9(6) VALUE 353363.
           02  FILLER                    PIC 9(6) VALUE 353464.
           02  FILLER                    PIC 9(6) VALUE 353465.
           02  FILLER                    PIC 9(6) VALUE 353566.
           02  FILLER                    PIC 9(6) VALUE 353667.
           02  FILLER                    PIC 9(6) VALUE 353768.
           02  FILLER                    PIC 9(6) VALUE 353800.
           02  FILLER                    PIC 9(6) VALUE 353888.
           02  FILLER                    PIC 9(6) VALUE 353889.
           02  FILLER                    PIC 9(6) VALUE 353890.
      *
           02  FILLER                    PIC 9(6) VALUE 363353.
           02  FILLER                    PIC 9(6) VALUE 363454.
           02  FILLER                    PIC 9(6) VALUE 363555.
           02  FILLER                    PIC 9(6) VALUE 363656.
           02  FILLER                    PIC 9(6) VALUE 363757.
           02  FILLER                    PIC 9(6) VALUE 363858.
           02  FILLER                    PIC 9(6) VALUE 363363.
           02  FILLER                    PIC 9(6) VALUE 363464.
           02  FILLER                    PIC 9(6) VALUE 363465.
           02  FILLER                    PIC 9(6) VALUE 363566.
           02  FILLER                    PIC 9(6) VALUE 363667.
           02  FILLER                    PIC 9(6) VALUE 363768.
           02  FILLER                    PIC 9(6) VALUE 363800.
           02  FILLER                    PIC 9(6) VALUE 363888.
           02  FILLER                    PIC 9(6) VALUE 363889.
           02  FILLER                    PIC 9(6) VALUE 363890.
      *
           02  FILLER                    PIC 9(6) VALUE 424525.
           02  FILLER                    PIC 9(6) VALUE 424626.
           02  FILLER                    PIC 9(6) VALUE 424454.
           02  FILLER                    PIC 9(6) VALUE 424555.
           02  FILLER                    PIC 9(6) VALUE 424656.
           02  FILLER                    PIC 9(6) VALUE 424757.
           02  FILLER                    PIC 9(6) VALUE 424858.
           02  FILLER                    PIC 9(6) VALUE 424464.
           02  FILLER                    PIC 9(6) VALUE 424465.
           02  FILLER                    PIC 9(6) VALUE 424566.
           02  FILLER                    PIC 9(6) VALUE 424667.
           02  FILLER                    PIC 9(6) VALUE 424768.
           02  FILLER                    PIC 9(6) VALUE 424800.
           02  FILLER                    PIC 9(6) VALUE 424888.
           02  FILLER                    PIC 9(6) VALUE 424889.
           02  FILLER                    PIC 9(6) VALUE 424890.
      *
           02  FILLER                    PIC 9(6) VALUE 454454.
           02  FILLER                    PIC 9(6) VALUE 454555.
           02  FILLER                    PIC 9(6) VALUE 454656.
           02  FILLER                    PIC 9(6) VALUE 454757.
           02  FILLER                    PIC 9(6) VALUE 454858.
           02  FILLER                    PIC 9(6) VALUE 454464.
           02  FILLER                    PIC 9(6) VALUE 454465.
           02  FILLER                    PIC 9(6) VALUE 454566.
           02  FILLER                    PIC 9(6) VALUE 454667.
           02  FILLER                    PIC 9(6) VALUE 454768.
           02  FILLER                    PIC 9(6) VALUE 454800.
           02  FILLER                    PIC 9(6) VALUE 454888.
           02  FILLER                    PIC 9(6) VALUE 454889.
           02  FILLER                    PIC 9(6) VALUE 454890.
      *
           02  FILLER                    PIC 9(6) VALUE 464454.
           02  FILLER                    PIC 9(6) VALUE 464555.
           02  FILLER                    PIC 9(6) VALUE 464656.
           02  FILLER                    PIC 9(6) VALUE 464757.
           02  FILLER                    PIC 9(6) VALUE 464858.
           02  FILLER                    PIC 9(6) VALUE 464464.
           02  FILLER                    PIC 9(6) VALUE 464465.
           02  FILLER                    PIC 9(6) VALUE 464566.
           02  FILLER                    PIC 9(6) VALUE 464667.
           02  FILLER                    PIC 9(6) VALUE 464768.
           02  FILLER                    PIC 9(6) VALUE 464800.
           02  FILLER                    PIC 9(6) VALUE 464888.
           02  FILLER                    PIC 9(6) VALUE 464889.
           02  FILLER                    PIC 9(6) VALUE 464890.
      *
           02  FILLER                    PIC 9(6) VALUE 465465.
           02  FILLER                    PIC 9(6) VALUE 465555.
           02  FILLER                    PIC 9(6) VALUE 465566.
           02  FILLER                    PIC 9(6) VALUE 465656.
           02  FILLER                    PIC 9(6) VALUE 465667.
           02  FILLER                    PIC 9(6) VALUE 465757.
           02  FILLER                    PIC 9(6) VALUE 465768.
           02  FILLER                    PIC 9(6) VALUE 465858.
           02  FILLER                    PIC 9(6) VALUE 465800.
           02  FILLER                    PIC 9(6) VALUE 465888.
           02  FILLER                    PIC 9(6) VALUE 465889.
           02  FILLER                    PIC 9(6) VALUE 465890.
      *
           02  FILLER                    PIC 9(6) VALUE 525626.
           02  FILLER                    PIC 9(6) VALUE 525555.
           02  FILLER                    PIC 9(6) VALUE 525656.
           02  FILLER                    PIC 9(6) VALUE 525757.
           02  FILLER                    PIC 9(6) VALUE 525858.
           02  FILLER                    PIC 9(6) VALUE 525566.
           02  FILLER                    PIC 9(6) VALUE 525667.
           02  FILLER                    PIC 9(6) VALUE 525768.
           02  FILLER                    PIC 9(6) VALUE 525800.
           02  FILLER                    PIC 9(6) VALUE 525888.
           02  FILLER                    PIC 9(6) VALUE 525889.
           02  FILLER                    PIC 9(6) VALUE 525890.
      *
           02  FILLER                    PIC 9(6) VALUE 555555.
           02  FILLER                    PIC 9(6) VALUE 555566.
           02  FILLER                    PIC 9(6) VALUE 555656.
           02  FILLER                    PIC 9(6) VALUE 555667.
           02  FILLER                    PIC 9(6) VALUE 555757.
           02  FILLER                    PIC 9(6) VALUE 555768.
           02  FILLER                    PIC 9(6) VALUE 555858.
           02  FILLER                    PIC 9(6) VALUE 555800.
           02  FILLER                    PIC 9(6) VALUE 555888.
           02  FILLER                    PIC 9(6) VALUE 555889.
           02  FILLER                    PIC 9(6) VALUE 555890.
      *
           02  FILLER                    PIC 9(6) VALUE 566555.
           02  FILLER                    PIC 9(6) VALUE 566566.
           02  FILLER                    PIC 9(6) VALUE 566656.
           02  FILLER                    PIC 9(6) VALUE 566667.
           02  FILLER                    PIC 9(6) VALUE 566757.
           02  FILLER                    PIC 9(6) VALUE 566768.
           02  FILLER                    PIC 9(6) VALUE 566858.
           02  FILLER                    PIC 9(6) VALUE 566800.
           02  FILLER                    PIC 9(6) VALUE 566888.
           02  FILLER                    PIC 9(6) VALUE 566889.
           02  FILLER                    PIC 9(6) VALUE 566890.
      *
           02  FILLER                    PIC 9(6) VALUE 626656.
           02  FILLER                    PIC 9(6) VALUE 626667.
           02  FILLER                    PIC 9(6) VALUE 626757.
           02  FILLER                    PIC 9(6) VALUE 626768.
           02  FILLER                    PIC 9(6) VALUE 626858.
           02  FILLER                    PIC 9(6) VALUE 626800.
           02  FILLER                    PIC 9(6) VALUE 626888.
           02  FILLER                    PIC 9(6) VALUE 626889.
           02  FILLER                    PIC 9(6) VALUE 626890.
      *
           02  FILLER                    PIC 9(6) VALUE 656757.
           02  FILLER                    PIC 9(6) VALUE 656768.
           02  FILLER                    PIC 9(6) VALUE 656858.
           02  FILLER                    PIC 9(6) VALUE 656800.
           02  FILLER                    PIC 9(6) VALUE 656888.
           02  FILLER                    PIC 9(6) VALUE 656889.
           02  FILLER                    PIC 9(6) VALUE 656890.
      *
300693     02  FILLER                    PIC 9(6) VALUE 667667.
           02  FILLER                    PIC 9(6) VALUE 667757.
           02  FILLER                    PIC 9(6) VALUE 667768.
           02  FILLER                    PIC 9(6) VALUE 667858.
           02  FILLER                    PIC 9(6) VALUE 667800.
           02  FILLER                    PIC 9(6) VALUE 667888.
           02  FILLER                    PIC 9(6) VALUE 667889.
           02  FILLER                    PIC 9(6) VALUE 667890.
      *
           02  FILLER                    PIC 9(6) VALUE 757757.
           02  FILLER                    PIC 9(6) VALUE 757768.
           02  FILLER                    PIC 9(6) VALUE 757858.
           02  FILLER                    PIC 9(6) VALUE 757800.
           02  FILLER                    PIC 9(6) VALUE 757888.
           02  FILLER                    PIC 9(6) VALUE 757889.
           02  FILLER                    PIC 9(6) VALUE 757890.
      *
           02  FILLER                    PIC 9(6) VALUE 768757.
           02  FILLER                    PIC 9(6) VALUE 768768.
           02  FILLER                    PIC 9(6) VALUE 768858.
           02  FILLER                    PIC 9(6) VALUE 768800.
           02  FILLER                    PIC 9(6) VALUE 768888.
           02  FILLER                    PIC 9(6) VALUE 768889.
           02  FILLER                    PIC 9(6) VALUE 768890.
      *
           02  FILLER                    PIC 9(6) VALUE 858800.
      *
           02  FILLER                    PIC 9(6) VALUE 888010.
           02  FILLER                    PIC 9(6) VALUE 888011.
           02  FILLER                    PIC 9(6) VALUE 888016.
           02  FILLER                    PIC 9(6) VALUE 888018.
           02  FILLER                    PIC 9(6) VALUE 888019.
           02  FILLER                    PIC 9(6) VALUE 888121.
           02  FILLER                    PIC 9(6) VALUE 888222.
           02  FILLER                    PIC 9(6) VALUE 888323.
           02  FILLER                    PIC 9(6) VALUE 888424.
           02  FILLER                    PIC 9(6) VALUE 888525.
           02  FILLER                    PIC 9(6) VALUE 888626.
           02  FILLER                    PIC 9(6) VALUE 888252.
           02  FILLER                    PIC 9(6) VALUE 888161.
           02  FILLER                    PIC 9(6) VALUE 888262.
           02  FILLER                    PIC 9(6) VALUE 888272.
           02  FILLER                    PIC 9(6) VALUE 888353.
           02  FILLER                    PIC 9(6) VALUE 888454.
           02  FILLER                    PIC 9(6) VALUE 888465.
           02  FILLER                    PIC 9(6) VALUE 888555.
           02  FILLER                    PIC 9(6) VALUE 888656.
           02  FILLER                    PIC 9(6) VALUE 888757.
           02  FILLER                    PIC 9(6) VALUE 888858.
           02  FILLER                    PIC 9(6) VALUE 888363.
           02  FILLER                    PIC 9(6) VALUE 888464.
           02  FILLER                    PIC 9(6) VALUE 888566.
           02  FILLER                    PIC 9(6) VALUE 888667.
           02  FILLER                    PIC 9(6) VALUE 888768.
           02  FILLER                    PIC 9(6) VALUE 888800.
           02  FILLER                    PIC 9(6) VALUE 888888.
           02  FILLER                    PIC 9(6) VALUE 888889.
           02  FILLER                    PIC 9(6) VALUE 888890.
      *
           02  FILLER                    PIC 9(6) VALUE 889010.
           02  FILLER                    PIC 9(6) VALUE 889011.
           02  FILLER                    PIC 9(6) VALUE 889016.
           02  FILLER                    PIC 9(6) VALUE 889018.
           02  FILLER                    PIC 9(6) VALUE 889019.
           02  FILLER                    PIC 9(6) VALUE 889121.
           02  FILLER                    PIC 9(6) VALUE 889222.
           02  FILLER                    PIC 9(6) VALUE 889323.
           02  FILLER                    PIC 9(6) VALUE 889424.
           02  FILLER                    PIC 9(6) VALUE 889525.
           02  FILLER                    PIC 9(6) VALUE 889626.
           02  FILLER                    PIC 9(6) VALUE 889252.
           02  FILLER                    PIC 9(6) VALUE 889161.
           02  FILLER                    PIC 9(6) VALUE 889262.
           02  FILLER                    PIC 9(6) VALUE 889272.
           02  FILLER                    PIC 9(6) VALUE 889353.
           02  FILLER                    PIC 9(6) VALUE 889454.
           02  FILLER                    PIC 9(6) VALUE 889465.
           02  FILLER                    PIC 9(6) VALUE 889555.
           02  FILLER                    PIC 9(6) VALUE 889656.
           02  FILLER                    PIC 9(6) VALUE 889757.
           02  FILLER                    PIC 9(6) VALUE 889858.
           02  FILLER                    PIC 9(6) VALUE 889363.
           02  FILLER                    PIC 9(6) VALUE 889464.
           02  FILLER                    PIC 9(6) VALUE 889566.
           02  FILLER                    PIC 9(6) VALUE 889667.
           02  FILLER                    PIC 9(6) VALUE 889768.
           02  FILLER                    PIC 9(6) VALUE 889800.
           02  FILLER                    PIC 9(6) VALUE 889888.
           02  FILLER                    PIC 9(6) VALUE 889889.
           02  FILLER                    PIC 9(6) VALUE 889890.
      *
           02  FILLER                    PIC 9(6) VALUE 890010.
           02  FILLER                    PIC 9(6) VALUE 890011.
           02  FILLER                    PIC 9(6) VALUE 890016.
           02  FILLER                    PIC 9(6) VALUE 890018.
           02  FILLER                    PIC 9(6) VALUE 890019.
           02  FILLER                    PIC 9(6) VALUE 890121.
           02  FILLER                    PIC 9(6) VALUE 890222.
           02  FILLER                    PIC 9(6) VALUE 890323.
           02  FILLER                    PIC 9(6) VALUE 890424.
           02  FILLER                    PIC 9(6) VALUE 890525.
           02  FILLER                    PIC 9(6) VALUE 890626.
           02  FILLER                    PIC 9(6) VALUE 890252.
           02  FILLER                    PIC 9(6) VALUE 890161.
           02  FILLER                    PIC 9(6) VALUE 890262.
           02  FILLER                    PIC 9(6) VALUE 890272.
           02  FILLER                    PIC 9(6) VALUE 890353.
           02  FILLER                    PIC 9(6) VALUE 890454.
           02  FILLER                    PIC 9(6) VALUE 890465.
           02  FILLER                    PIC 9(6) VALUE 890555.
           02  FILLER                    PIC 9(6) VALUE 890656.
           02  FILLER                    PIC 9(6) VALUE 890757.
           02  FILLER                    PIC 9(6) VALUE 890858.
           02  FILLER                    PIC 9(6) VALUE 890363.
           02  FILLER                    PIC 9(6) VALUE 890464.
           02  FILLER                    PIC 9(6) VALUE 890566.
           02  FILLER                    PIC 9(6) VALUE 890667.
           02  FILLER                    PIC 9(6) VALUE 890768.
           02  FILLER                    PIC 9(6) VALUE 890800.
           02  FILLER                    PIC 9(6) VALUE 890888.
           02  FILLER                    PIC 9(6) VALUE 890889.
           02  FILLER                    PIC 9(6) VALUE 890890.
      *
           02  FILLER                    PIC 9(6) VALUE ZERO.
           02  FILLER                    PIC X(6) VALUE 'END'.
      *
       01  T2-BETREUUNGS-TABR  REDEFINES T2-BETREUUNGS-TAB.
           02  T2-BETREU-TAB1    OCCURS 698.
               05  T2-BETREUTER   PIC 999.
               05  T2-BETREUER    PIC 999.
       01  T2-BETREUUNGS-TABR2 REDEFINES T2-BETREUUNGS-TAB.
           02  T2-BETREU-TAB2    OCCURS 698.
               05  T2-ALPH        PIC XXX.
               05  FILLER         PIC XXX.

