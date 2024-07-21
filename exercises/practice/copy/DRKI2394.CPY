      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  22.03.2010
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2010 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI2394.CPY  $
      *    $Revision:   2.4  $
      *
      *    $Log:   L:/mf/CPY/DRKI2394.CPv  $
      *
      *   Rev 2.4   Sep 20 2012 09:56:12   t13ae49
      *AKI00886: NKDA: Upros VN / BZ / VP insert, update, delete
      *
      *   Rev 2.3   Sep 06 2012 14:32:54   t13ae49
      *AKI00886: NKDA: Upros VN / BZ / VP insert, update, delete
      *
      *   Rev 2.2   Sep 06 2012 11:21:48   t13ex14
      *AKI00886: NKDA: Upros VN / BZ / VP insert, update, delete
      *
      *   Rev 2.1   Sep 06 2012 11:09:04   t13ex14
      *AKI00886: NKDA: Upros VN / BZ / VP insert, update, delete
      *
      *   Rev 2.0   Mar 22 2010 12:41:00   t13ex14
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
      *
       01  PI-DRKI2394.

           05  PI-DRKI2394-KEY.

             10 PI-DRKI2394-VERTRAGSID     PIC  X(013).
             10 PI-DRKI2394-PERSONLFDNR    PIC  9(002).
             10 PI-DRKI2394-RUF-PGM        PIC  X(008).


           05  PI-DRKI2394-DATEN.

             10 PI-DRKI2394-GES            PIC  9(002).
             10 PI-DRKI2394-VNR            PIC  9(015).
             10 PI-DRKI2394-ADRESSEID      PIC  X(013).
             10 PI-DRKI2394-PERSONENID     PIC  X(013).
      *      10 PI-DRKI2394-ROLLE-VERTRAG  PIC  X(001).
      *      10 PI-DRKI2394-ROLLE-TARIF    PIC  X(001).
             10 PI-DRKI2394-PERSROLLEKZ    PIC  X(002).

             10 PI-DRKI2394-ANREDE         PIC S9(002).
             10 PI-DRKI2394-NNAME          PIC  X(030).
             10 PI-DRKI2394-VNAME          PIC  X(030).
             10 PI-DRKI2394-GEBDAT         PIC  X(010).
             10 PI-DRKI2394-BEGINNDATUM    PIC  X(010).
             10 PI-DRKI2394-ENDEDATUM      PIC  X(010).


       01  PO-DRKI2394.

           05  PO-DRKI2394-RC              PIC  X(003).
               88 PO-DRKI2394-RC-OK              VALUE 'OK '.
               88 PO-DRKI2394-RC-NOT-FOUND       VALUE 'NFD'.
               88 PO-DRKI2394-RC-NOK             VALUE 'NOK'.
               88 PO-DRKI2394-RC-TU-NICHTS       VALUE 'NOP'.

           05  PO-DRKI2394-ROLLE           PIC  X(002).
               88 PO-DRKI2394-ROLLE-NP           VALUE 'NP'.
               88 PO-DRKI2394-ROLLE-N            VALUE 'N '.
               88 PO-DRKI2394-ROLLE-P            VALUE ' P'.
      *
      *    für Log-Datei des Hauptprogramms:
           05  PO-DRKI2394-FEHLERSATZ.
               10 PO-DRKI2394-FEHLER-FUNKTION    PIC  X(10).
               10 PO-DRKI2394-FEHLER-TABELLE     PIC  X(20).
               10 PO-DRKI2394-FEHLER-SQL-FEHLER  PIC S9(04).
               10 PO-DRKI2394-FEHLER-KEY         PIC  X(40).
               10 FILLER REDEFINES PO-DRKI2394-FEHLER-KEY.
                  15 PO-DRKI2394-GES             PIC  9(002).
                  15 PO-DRKI2394-VNR             PIC  X(015).
      ******* ENDE COPY-MEMBER ***************************************
