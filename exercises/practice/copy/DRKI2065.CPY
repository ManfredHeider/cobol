      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  23.09.2016
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2016 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI2065.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRKI2065.CPv  $
      *
      *   Rev 2.2   Sep 27 2016 10:55:54   t13ex73
      *AKI01472: Aktpgm für Tabelle EINMALZAHLUNGEN vereinheitlichen
      *
      *   Rev 2.0   Sep 23 2016 10:37:05 T13EX73
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI2065                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI2065                    *
      *    --------------------------------------------------------   *
      ****************************************************************
      *   ERZEUGT MIT OZPGEN         23.09.2016 / 10:37:05 / T13EX73
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
      *    FÜR DIE FUNKTION DRKI2065
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  INSERT  <<<
      *
      ****************************************************************
      *
       01  PI-DRKI2065.
           05  PI-DRKI2065-KEY.
 
ANF   *        %M_PI_KEY%
             10 PI-DRKI2065-EMZVERTRAGSID   PIC  X(0013).
END   *
 
           05  PI-DRKI2065-NI.
 
ANF   *        %M_PI_DATEN_NI%
             15 FILLER                         PIC S9(4) COMP.
END   *
 
           05  PI-DRKI2065-DATEN.
 
ANF   *        %M_PI_DATEN%
             10 PI-DRKI2065-EMZABSCHLUSSVBNR   PIC S9(9) COMP.
             10 PI-DRKI2065-EMZASCLUBMONLF   PIC S9(4) COMP.
             10 PI-DRKI2065-EMZBEITRAGMON1   PIC S9(9)V9(2) COMP-3.
             10 PI-DRKI2065-EMZBEITRAGMON10   PIC S9(9)V9(2) COMP-3.
             10 PI-DRKI2065-EMZBEITRAGMON11   PIC S9(9)V9(2) COMP-3.
             10 PI-DRKI2065-EMZBEITRAGMON12   PIC S9(9)V9(2) COMP-3.
             10 PI-DRKI2065-EMZBEITRAGMON2   PIC S9(9)V9(2) COMP-3.
             10 PI-DRKI2065-EMZBEITRAGMON3   PIC S9(9)V9(2) COMP-3.
             10 PI-DRKI2065-EMZBEITRAGMON4   PIC S9(9)V9(2) COMP-3.
             10 PI-DRKI2065-EMZBEITRAGMON5   PIC S9(9)V9(2) COMP-3.
             10 PI-DRKI2065-EMZBEITRAGMON6   PIC S9(9)V9(2) COMP-3.
             10 PI-DRKI2065-EMZBEITRAGMON7   PIC S9(9)V9(2) COMP-3.
             10 PI-DRKI2065-EMZBEITRAGMON8   PIC S9(9)V9(2) COMP-3.
             10 PI-DRKI2065-EMZBEITRAGMON9   PIC S9(9)V9(2) COMP-3.
             10 PI-DRKI2065-EMZBEITRMONSTAND   PIC S9(4) COMP.
             10 PI-DRKI2065-EMZERSTELLDATUM   PIC  X(010).
             10 PI-DRKI2065-EMZGESAMTBEITRAG   PIC S9(9)V9(2) COMP-3.
             10 PI-DRKI2065-EMZKOMPLETTVERT   PIC  X(0001).
             10 PI-DRKI2065-EMZLZTMONBEITRAG   PIC S9(9)V9(2) COMP-3.
             10 PI-DRKI2065-EMZMONATSBEITRAG   PIC S9(9)V9(2) COMP-3.
             10 PI-DRKI2065-EMZURSPVERTREND   PIC  X(010).
             10 PI-DRKI2065-EMZURSRVERTRBEG   PIC  X(010).
             10 PI-DRKI2065-EMZVERTRAGSNR   PIC  X(0015).
             10 PI-DRKI2065-EMZZAHLUNGSWEISN   PIC S9(4) COMP.
             10 PI-DRKI2065-EMZZAHLUNGSWEISU   PIC S9(4) COMP.
END   *
 
      *  INITIALISIERUNG NULL-INDIKATOR
       01  PI-DRKI2065-NI1.
           05 PI-DRKI2065-INIT-TAB1 PIC S9(04) COMP VALUE -1
                                      OCCURS 1.
       01  PI-DRKI2065-NI7.
           05 PI-DRKI2065-INIT-TAB7 PIC S9(04) COMP VALUE -7
                                      OCCURS 1.
 
      *
       01  PO-DRKI2065.
           05  PO-DRKI2065-RC                     PIC  X(003).
               88 PO-DRKI2065-RC-OK                  VALUE 'OK '.
               88 PO-DRKI2065-RC-DUPLICATE-VALUE VALUE 'DVA'.
      *
      ******* ENDE COPY-MEMBER **************************************

