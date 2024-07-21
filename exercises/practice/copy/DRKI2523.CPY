      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  23.08.2011
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2011 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI2523.CPY  $
      *    $Revision:   2.4  $
      *
      *    $Log:   L:/mf/CPY/DRKI2523.CPv  $
      *
      *   Rev 2.4   Sep 20 2012 09:57:08   t13ae49
      *AKI00886: NKDA: Upros VN / BZ / VP insert, update, delete
      *
      *   Rev 2.3   Sep 06 2012 14:33:50   t13ae49
      *AKI00886: NKDA: Upros VN / BZ / VP insert, update, delete
      *
      *   Rev 2.2   Sep 06 2012 11:23:18   t13ex14
      *AKI00886: NKDA: Upros VN / BZ / VP insert, update, delete
      *
      *   Rev 2.1   Sep 06 2012 11:10:22   t13ex14
      *AKI00886: NKDA: Upros VN / BZ / VP insert, update, delete
      *
      *   Rev 2.0   Aug 23 2011 10:47:46   t13ex14
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI2394                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI2394                    *
      *    --------------------------------------------------------   *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AKI00943!22.03.10! 2.1! Neu                       ! WMI      *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER
      *    FÜR DIE FUNKTION DRKI2394
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  UPDATE KRANKENPERSEIGEN <<<
      *
      ****************************************************************
       01  PI-DRKI2523.
           05  PI-DRKI2523-KEY.

             10 PI-DRKI2523-PERSONLFDNR   PIC S9(4) COMP.
             10 PI-DRKI2523-VERTRAGSID    PIC  X(0013).
             10 PI-DRKI2523-RUF-PGM       PIC  X(008).

           05  PI-DRKI2523-DATEN.

             10 PI-DRKI2523-GES           PIC  9(002).
             10 PI-DRKI2523-VNR           PIC  X(015).
             10 PI-DRKI2523-BEGINNDATUM   PIC  X(010).
             10 PI-DRKI2523-ENDEDATUM     PIC  X(010).
      *
       01  PO-DRKI2523.
           05  PO-DRKI2523-RC                     PIC  X(003).
               88 PO-DRKI2523-RC-OK               VALUE 'OK '.
               88 PO-DRKI2523-RC-NOK              VALUE 'NOK'.
               88 PO-DRKI2523-RC-NOT-FOUND        VALUE 'NFD'.
      *
      *    für Log-Datei des Hauptprogramms:
           05  PO-DRKI2523-FEHLERSATZ.
               10 PO-DRKI2523-FEHLER-FUNKTION    PIC  X(10).
               10 PO-DRKI2523-FEHLER-TABELLE     PIC  X(20).
               10 PO-DRKI2523-FEHLER-SQL-FEHLER  PIC S9(04).
               10 PO-DRKI2523-FEHLER-KEY         PIC  X(40).
