      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  23.08.2007
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2016 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI2066.CPY  $                                 00010000
 
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRKI2066.CPv  $
      *
      *   Rev 2.2   Sep 27 2016 10:56:00   t13ex73
      *AKI01472: Aktpgm für Tabelle EINMALZAHLUNGEN vereinheitlichen
      *
      *   Rev 2.1   Oct 10 2007 10:18:02   t13ex50
      *AKI00630: Einmalbeitragsversicherungen in der AS-Club-Wertung
      *
      *   Rev 2.0   Aug 23 2007 09:00:48   t13ex50
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI2066                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI2066                    *
      *    --------------------------------------------------------   *
      ****************************************************************
      *   ERZEUGT MIT OZPGEN         23.09.2016 / 10:37:07 / T13EX73
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
      *    FÜR DIE FUNKTION DRKI2066
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  UPDATE  <<<
      *
      ****************************************************************
      *
       01  PI-DRKI2066.
           05  PI-DRKI2066-KEY.
 
 
ANF   *        %M_PI_KEY%
             10 PI-DRKI2066-EMZVERTRAGSID   PIC  X(0013).
END   *
 
           05  PI-DRKI2066-NI.
 
ANF   *        %M_PI_DATEN_NI%
             15 FILLER                         PIC S9(4) COMP.
END   *
 
           05  PI-DRKI2066-DATEN.
 
ANF   *        %M_PI_DATEN%
             10 PI-DRKI2066-EMZABSCHLUSSVBNR   PIC S9(9) COMP.
             10 PI-DRKI2066-EMZASCLUBMONLF   PIC S9(4) COMP.
             10 PI-DRKI2066-EMZBEITRAGMON1   PIC S9(9)V9(2) COMP-3.
             10 PI-DRKI2066-EMZBEITRAGMON10   PIC S9(9)V9(2) COMP-3.
             10 PI-DRKI2066-EMZBEITRAGMON11   PIC S9(9)V9(2) COMP-3.
             10 PI-DRKI2066-EMZBEITRAGMON12   PIC S9(9)V9(2) COMP-3.
             10 PI-DRKI2066-EMZBEITRAGMON2   PIC S9(9)V9(2) COMP-3.
             10 PI-DRKI2066-EMZBEITRAGMON3   PIC S9(9)V9(2) COMP-3.
             10 PI-DRKI2066-EMZBEITRAGMON4   PIC S9(9)V9(2) COMP-3.
             10 PI-DRKI2066-EMZBEITRAGMON5   PIC S9(9)V9(2) COMP-3.
             10 PI-DRKI2066-EMZBEITRAGMON6   PIC S9(9)V9(2) COMP-3.
             10 PI-DRKI2066-EMZBEITRAGMON7   PIC S9(9)V9(2) COMP-3.
             10 PI-DRKI2066-EMZBEITRAGMON8   PIC S9(9)V9(2) COMP-3.
             10 PI-DRKI2066-EMZBEITRAGMON9   PIC S9(9)V9(2) COMP-3.
             10 PI-DRKI2066-EMZBEITRMONSTAND   PIC S9(4) COMP.
             10 PI-DRKI2066-EMZERSTELLDATUM   PIC  X(010).
             10 PI-DRKI2066-EMZGESAMTBEITRAG   PIC S9(9)V9(2) COMP-3.
             10 PI-DRKI2066-EMZKOMPLETTVERT   PIC  X(0001).
             10 PI-DRKI2066-EMZLZTMONBEITRAG   PIC S9(9)V9(2) COMP-3.
             10 PI-DRKI2066-EMZMONATSBEITRAG   PIC S9(9)V9(2) COMP-3.
             10 PI-DRKI2066-EMZURSPVERTREND   PIC  X(010).
             10 PI-DRKI2066-EMZURSRVERTRBEG   PIC  X(010).
             10 PI-DRKI2066-EMZVERTRAGSNR   PIC  X(0015).
             10 PI-DRKI2066-EMZZAHLUNGSWEISN   PIC S9(4) COMP.
             10 PI-DRKI2066-EMZZAHLUNGSWEISU   PIC S9(4) COMP.
END   *
 
      *  INITIALISIERUNG NULL-INDIKATOR
       01  PI-DRKI2066-NI1.
           05 PI-DRKI2066-INIT-TAB1 PIC S9(04) COMP VALUE -1
                                  OCCURS 1.
       01  PI-DRKI2066-NI7.
           05 PI-DRKI2066-INIT-TAB7 PIC S9(04) COMP VALUE -7
                                  OCCURS 1.
      *
       01  PO-DRKI2066.
           05  PO-DRKI2066-RC                     PIC  X(003).
               88 PO-DRKI2066-RC-OK               VALUE 'OK '.
               88 PO-DRKI2066-RC-NOT-FOUND        VALUE 'NFD'.
      *
      ******* ENDE COPY-MEMBER ***************************************

