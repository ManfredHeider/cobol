      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  20.05.2020
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2020 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI3795.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRKI3795.CPv  $
      *
      *   Rev 2.2   Jul 07 2020 12:32:20   t13ex78
      *AKI01780: Standard OZP G90EVERTRAG
      *
      *   Rev 2.1   Jun 12 2020 11:43:44   t13ex78
      *AKI01780: Standard OZP G90EVERTRAG
      *
      *   Rev 2.0   May 20 2020 08:51:12 T13EX78
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI3795                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI3795                    *
      *    --------------------------------------------------------   *
      ****************************************************************
      *   ERZEUGT MIT OZPGEN         18.06.2020 / 08:02:09 / T13EX78
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
      *    FÜR DIE FUNKTION DRKI3795
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  INSERT  <<<
      *
      ****************************************************************
      *
       01  PI-DRKI3795.
           05  PI-DRKI3795-KEY.
 
ANF   *        %M_PI_KEY%
             10 PI-DRKI3795-G90EVERTRAGSNR   PIC  X(0015).
END   *
 
           05  PI-DRKI3795-NI.
 
ANF   *        %M_PI_DATEN_NI%
             15 FILLER                          PIC S9(4) COMP.
END   *
 
           05  PI-DRKI3795-DATEN.
 
ANF   *        %M_PI_DATEN%
             10 PI-DRKI3795-G90EABSCHLVBNR   PIC S9(9) COMP.
             10 PI-DRKI3795-G90EBISDATJAHR   PIC  X(0008).
             10 PI-DRKI3795-G90EBISDATMONAT   PIC  X(0008).
             10 PI-DRKI3795-G90EEINRICHTGEB   PIC S9(8)V9(2) COMP-3.
             10 PI-DRKI3795-G90EFACTANKRAHMEN   PIC S9(10)V9(2) COMP-3.
             10 PI-DRKI3795-G90EFACTGEBPROZ   PIC S9(1)V9(2) COMP-3.
             10 PI-DRKI3795-G90EGEBNICHTANG   PIC S9(1)V9(2) COMP-3.
             10 PI-DRKI3795-G90EIBAN   PIC  X(0034).
             10 PI-DRKI3795-G90EINFOSPERREKZN   PIC  X(0001).
             10 PI-DRKI3795-G90EMANDANT   PIC S9(9) COMP.
             10 PI-DRKI3795-G90EPRODUKTART   PIC S9(1)V COMP-3.
             10 PI-DRKI3795-G90ERECHNUNGJAHR   PIC S9(8)V COMP-3.
             10 PI-DRKI3795-G90ERECHNUNGMONAT   PIC S9(8)V COMP-3.
             10 PI-DRKI3795-G90EUMSATZJAHR   PIC S9(8)V9(2) COMP-3.
             10 PI-DRKI3795-G90EUMSATZMONAT   PIC S9(8)V9(2) COMP-3.
             10 PI-DRKI3795-G90EVERTRAGBEGINN   PIC  X(0008).
             10 PI-DRKI3795-G90EVERTRAGENDE   PIC  X(0008).
             10 PI-DRKI3795-G90EVONDATJAHR   PIC  X(0008).
             10 PI-DRKI3795-G90EVONDATMONAT   PIC  X(0008).
             10 PI-DRKI3795-G90EZUSTAND   PIC  X(0001).
END   *
 
      *  INITIALISIERUNG NULL-INDIKATOR
       01  PI-DRKI3795-NI1.
           05 PI-DRKI3795-INIT-TAB1 PIC S9(04) COMP VALUE -1
                                      OCCURS 1.
       01  PI-DRKI3795-NI7.
           05 PI-DRKI3795-INIT-TAB7 PIC S9(04) COMP VALUE -7
                                      OCCURS 1.
 
      *
       01  PO-DRKI3795.
           05  PO-DRKI3795-RC                     PIC  X(003).
               88 PO-DRKI3795-RC-OK                  VALUE 'OK '.
               88 PO-DRKI3795-RC-DUPLICATE-VALUE VALUE 'DVA'.
      *
      ******* ENDE COPY-MEMBER **************************************

