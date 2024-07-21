      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  21.04.2011
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2011 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI2457.CPY  $
      *    $Revision:   2.4  $
      *
      *    $Log:   L:/mf/CPY/DRKI2457.CPv  $
      *
      *   Rev 2.4   Sep 20 2012 09:57:02   t13ae49
      *AKI00886: NKDA: Upros VN / BZ / VP insert, update, delete
      *
      *   Rev 2.3   Sep 06 2012 14:33:44   t13ae49
      *AKI00886: NKDA: Upros VN / BZ / VP insert, update, delete
      *
      *   Rev 2.2   Sep 06 2012 11:23:08   t13ex14
      *AKI00886: NKDA: Upros VN / BZ / VP insert, update, delete
      *
      *   Rev 2.1   Sep 06 2012 11:10:14   t13ex14
      *AKI00886: NKDA: Upros VN / BZ / VP insert, update, delete
      *
      *   Rev 2.0   Apr 21 2011 07:55:40   t13ex14
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI2457                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI2457                    *
      *    --------------------------------------------------------   *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AKI00866!21.04.11! 2.1! Neu                       ! WMI      *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER
      *    FÜR DIE FUNKTION DRKI2457
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  DELETE <<<
      *
      ****************************************************************
      *
       01  PI-DRKI2457.
           05  PI-DRKI2457-KEY.
             10 PI-DRKI2457-GES                  PIC  9(002).
             10 PI-DRKI2457-VERTRAGSID           PIC  X(013).
             10 PI-DRKI2457-VERTRAGSNUMMER       PIC  X(015).
             10 PI-DRKI2457-PERSONENID           PIC  X(015).
             10 PI-DRKI2457-VBPERSID             PIC  X(015).

           05  PI-DRKI2457-DATEN.
             10 PI-DRKI2457-ANZNAME              PIC  X(030).
             10 PI-DRKI2457-ANZVORNAME           PIC  X(030).
             10 PI-DRKI2457-BESTANDFKT-KZ        PIC  X(001).
                88 S-BESTANDFKT-NEIN             VALUE ' ', 'N'.
                88 S-BESTANDFKT-JA               VALUE 'J'.

      *
       01  PO-DRKI2457.
           05  PO-DRKI2457-RC                    PIC  X(003).
               88 PO-DRKI2457-RC-OK              VALUE 'OK '.
               88 PO-DRKI2457-RC-NOT-FOUND       VALUE 'NFD'.
               88 PO-DRKI2457-RC-NOK             VALUE 'NOK'.
               88 PO-DRKI2457-RC-TU-NICHTS       VALUE 'NOP'.

      *    für Log-Datei des Hauptprogramms:
           05  PO-DRKI2457-FEHLERSATZ.
               10 PO-DRKI2457-FUNKTION           PIC  X(10).
               10 PO-DRKI2457-TABELLE            PIC  X(20).
               10 PO-DRKI2457-SQL-FEHLER         PIC S9(04).
               10 PO-DRKI2457-KEY                PIC  X(40).
               10 FILLER REDEFINES PO-DRKI2457-KEY.
                  15 PO-DRKI2457-GES             PIC  9(002).
                  15 PO-DRKI2457-VNR             PIC  X(015).
      *
      ******* ENDE COPY-MEMBER **************************************
