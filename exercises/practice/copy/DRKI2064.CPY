      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  23.08.2007
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2016 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI2064.CPY  $                                 00010000
 
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRKI2064.CPv  $
      *
      *   Rev 2.2   Sep 27 2016 10:55:50   t13ex73
      *AKI01472: Aktpgm für Tabelle EINMALZAHLUNGEN vereinheitlichen
      *
      *   Rev 2.1   Oct 10 2007 10:17:48   t13ex50
      *AKI00630: Einmalbeitragsversicherungen in der AS-Club-Wertung
      *
      *   Rev 2.0   Aug 23 2007 08:58:06   t13ex50
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI2064                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI2064                    *
      *    --------------------------------------------------------   *
      ****************************************************************
      *   ERZEUGT MIT OZPGEN         23.09.2016 / 10:37:03 / T13EX73
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
      *    FÜR DIE FUNKTION DRKI2064
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  SELECT  <<<
      *
      ****************************************************************
      *
       01  PI-DRKI2064.
           05  PI-DRKI2064-KEY.
 
 
ANF   *        %M_PI_KEY%
             10 PI-DRKI2064-EMZVERTRAGSID   PIC  X(0013).
END   *
 
      *
       01  PO-DRKI2064.
           05  PO-DRKI2064-RC                     PIC  X(003).
               88 PO-DRKI2064-RC-OK               VALUE 'OK '.
               88 PO-DRKI2064-RC-NOT-FOUND        VALUE 'NFD'.
      *
ANF   *        %M_PO_DATEN_NI%
END   *
 
           05  PO-DRKI2064-DATEN.
 
 
ANF   *        %M_PO_DATEN%
             10 PO-DRKI2064-EMZABSCHLUSSVBNR   PIC S9(9) COMP.
             10 PO-DRKI2064-EMZASCLUBMONLF   PIC S9(4) COMP.
             10 PO-DRKI2064-EMZBEITRAGMON1   PIC S9(9)V9(2) COMP-3.
             10 PO-DRKI2064-EMZBEITRAGMON10   PIC S9(9)V9(2) COMP-3.
             10 PO-DRKI2064-EMZBEITRAGMON11   PIC S9(9)V9(2) COMP-3.
             10 PO-DRKI2064-EMZBEITRAGMON12   PIC S9(9)V9(2) COMP-3.
             10 PO-DRKI2064-EMZBEITRAGMON2   PIC S9(9)V9(2) COMP-3.
             10 PO-DRKI2064-EMZBEITRAGMON3   PIC S9(9)V9(2) COMP-3.
             10 PO-DRKI2064-EMZBEITRAGMON4   PIC S9(9)V9(2) COMP-3.
             10 PO-DRKI2064-EMZBEITRAGMON5   PIC S9(9)V9(2) COMP-3.
             10 PO-DRKI2064-EMZBEITRAGMON6   PIC S9(9)V9(2) COMP-3.
             10 PO-DRKI2064-EMZBEITRAGMON7   PIC S9(9)V9(2) COMP-3.
             10 PO-DRKI2064-EMZBEITRAGMON8   PIC S9(9)V9(2) COMP-3.
             10 PO-DRKI2064-EMZBEITRAGMON9   PIC S9(9)V9(2) COMP-3.
             10 PO-DRKI2064-EMZBEITRMONSTAND   PIC S9(4) COMP.
             10 PO-DRKI2064-EMZERSTELLDATUM   PIC  X(010).
             10 PO-DRKI2064-EMZGESAMTBEITRAG   PIC S9(9)V9(2) COMP-3.
             10 PO-DRKI2064-EMZKOMPLETTVERT   PIC  X(0001).
             10 PO-DRKI2064-EMZLZTMONBEITRAG   PIC S9(9)V9(2) COMP-3.
             10 PO-DRKI2064-EMZMONATSBEITRAG   PIC S9(9)V9(2) COMP-3.
             10 PO-DRKI2064-EMZURSPVERTREND   PIC  X(010).
             10 PO-DRKI2064-EMZURSRVERTRBEG   PIC  X(010).
             10 PO-DRKI2064-EMZVERTRAGSNR   PIC  X(0015).
             10 PO-DRKI2064-EMZZAHLUNGSWEISN   PIC S9(4) COMP.
             10 PO-DRKI2064-EMZZAHLUNGSWEISU   PIC S9(4) COMP.
END   *
 
 
      *
      ******* ENDE COPY-MEMBER ***************************************

