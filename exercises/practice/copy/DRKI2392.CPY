      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  22.03.2010
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2010 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI2392.CPY  $
      *    $Revision:   2.4  $
      *
      *    $Log:   L:/mf/CPY/DRKI2392.CPv  $
      *
      *   Rev 2.4   Sep 20 2012 09:56:00   t13ae49
      *AKI00886: NKDA: Upros VN / BZ / VP insert, update, delete
      *
      *   Rev 2.3   Sep 06 2012 14:32:42   t13ae49
      *AKI00886: NKDA: Upros VN / BZ / VP insert, update, delete
      *
      *   Rev 2.2   Sep 06 2012 11:21:28   t13ex14
      *AKI00886: NKDA: Upros VN / BZ / VP insert, update, delete
      *
      *   Rev 2.1   Sep 06 2012 11:08:48   t13ex14
      *AKI00886: NKDA: Upros VN / BZ / VP insert, update, delete
      *
      *   Rev 2.0   Mar 22 2010 12:40:48   t13ex14
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI2392                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI2392                    *
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
      *    FÜR DIE FUNKTION DRKI2392
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  SELECT KRANKENPERSEIGEN <<<
      *
      ****************************************************************
      *
       01  PI-DRKI2392.
           05  PI-DRKI2392-KEY.
             10 PI-DRKI2392-VERTRAGSID     PIC  X(013).
             10 PI-DRKI2392-PERSONLFDNR    PIC  9(002).
             10 PI-DRKI2392-RUF-PGM        PIC  X(008).

       01  PO-DRKI2392.

           05  PO-DRKI2392-DATEN.
             10 PI-DRKI2392-PRODUKTID      PIC  X(013).
             10 PO-DRKI2392-ANREDE         PIC S9(002).
             10 PO-DRKI2392-NNAME          PIC  X(030).
             10 PO-DRKI2392-VNAME          PIC  X(030).
             10 PO-DRKI2392-GEBDAT         PIC  X(010).
             10 PI-DRKI2392-PERSONENID     PIC  X(013).
             10 PI-DRKI2392-ADRESSEID      PIC  X(013).
             10 PI-DRKI2392-BEGINNDATUM    PIC  X(010).
             10 PI-DRKI2392-ENDEDATUM      PIC  X(010).
             10 PI-DRKI2392-PERSONENNUMMER PIC  9(002).
             10 PI-DRKI2392-GESCHSTID      PIC  9(002).
      *
           05  PO-DRKI2392-RC              PIC  X(003).

               88 PO-DRKI2392-RC-OK              VALUE 'OK '.
               88 PO-DRKI2392-RC-NOT-FOUND       VALUE 'NFD'.
               88 PO-DRKI2392-RC-NOK             VALUE 'NOK'.
               88 PO-DRKI2392-RC-TU-NICHTS       VALUE 'NOP'.
      *
      *    für Log-Datei des Hauptprogramms:
           05  PO-DRKI2392-FEHLERSATZ.
               10 PO-DRKI2392-FEHLER-FUNKTION    PIC  X(10).
               10 PO-DRKI2392-FEHLER-TABELLE     PIC  X(20).
               10 PO-DRKI2392-FEHLER-SQL-FEHLER  PIC S9(04).
               10 PO-DRKI2392-FEHLER-KEY         PIC  X(40).
               10 FILLER REDEFINES PO-DRKI2392-FEHLER-KEY.
                  15 PO-DRKI2392-GES             PIC  9(002).
                  15 PO-DRKI2392-VNR             PIC  X(015).
      ******* ENDE COPY-MEMBER **************************************
