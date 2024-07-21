      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  22.08.2012
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2018 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI2661.CPY  $
      *    $Revision:   2.3  $
      *
      *    $Log:   L:/mf/CPY/DRKI2661.CPv  $
      *
      *   Rev 2.3   Oct 22 2018 08:13:16   t13ae31
      *AKI01630: CKV Bordero neues Feld OZP
      *
      *   Rev 2.2   Oct 18 2012 10:26:22   T13EX20
      *AKI01147: Central-Tarifumstellung (Bordero-Tool)
      *
      *   Rev 2.1   Sep 26 2012 10:12:48   t13ex52
      *AKI01147: Central-Tarifumstellung (Bordero-Tool)
      *
      *   Rev 2.0   Aug 22 2012 17:36:51 T13EX52
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI2661                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI2661                    *
      *    --------------------------------------------------------   *
      ****************************************************************
      *   ERZEUGT MIT OZPGEN         19.09.2018 / 09:29:22 / T13AE31
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER
      *    FÜR DIE FUNKTION DRKI2661
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  INSERT  <<<
      *
      ****************************************************************
      *
       01  PI-DRKI2661.
           05  PI-DRKI2661-KEY.
 
ANF   *        %M_PI_KEY%
             10 PI-DRKI2661-VPSPERSONENNR   PIC S9(4) COMP.
             10 PI-DRKI2661-VPSVERTRAGSID   PIC  X(0013).
END   *
 
           05  PI-DRKI2661-NI.
 
ANF   *        %M_PI_DATEN_NI%
             15 FILLER                          PIC S9(4) COMP.
END   *
 
           05  PI-DRKI2661-DATEN.
 
ANF   *        %M_PI_DATEN%
             10 PI-DRKI2661-VPSBEITRVORTEIL   PIC S9(7)V9(2) COMP-3.
             10 PI-DRKI2661-VPSKUNDBINDPERS   PIC  X(0001).
             10 PI-DRKI2661-VPSPERSBEITRRUEK   PIC S9(7)V9(2) COMP-3.
             10 PI-DRKI2661-VPSPERSERHOEHPRZ   PIC S9(7)V9(2) COMP-3.
             10 PI-DRKI2661-VPSPERSRUECKSTLG   PIC S9(7)V9(2) COMP-3.
END   *
 
      *  INITIALISIERUNG NULL-INDIKATOR
       01  PI-DRKI2661-NI1.
           05 PI-DRKI2661-INIT-TAB1 PIC S9(04) COMP VALUE -1
                                      OCCURS 1.
       01  PI-DRKI2661-NI7.
           05 PI-DRKI2661-INIT-TAB7 PIC S9(04) COMP VALUE -7
                                      OCCURS 1.
 
      *
       01  PO-DRKI2661.
           05  PO-DRKI2661-RC                     PIC  X(003).
               88 PO-DRKI2661-RC-OK                  VALUE 'OK '.
               88 PO-DRKI2661-RC-DUPLICATE-VALUE VALUE 'DVA'.
      *
      ******* ENDE COPY-MEMBER **************************************

