      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  28.11.2011
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2011 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI2543.CPY  $
      *    $Revision:   2.4  $
      *
      *    $Log:   L:/mf/CPY/DRKI2543.CPv  $
      *
      *   Rev 2.4   Sep 20 2012 09:57:16   t13ae49
      *AKI00886: NKDA: Upros VN / BZ / VP insert, update, delete
      *
      *   Rev 2.3   Sep 06 2012 14:33:56   t13ae49
      *AKI00886: NKDA: Upros VN / BZ / VP insert, update, delete
      *
      *   Rev 2.2   Sep 06 2012 11:23:30   t13ex14
      *AKI00886: NKDA: Upros VN / BZ / VP insert, update, delete
      *
      *   Rev 2.1   Sep 06 2012 11:10:32   t13ex14
      *AKI00886: NKDA: Upros VN / BZ / VP insert, update, delete
      *
      *   Rev 2.0   Nov 28 2011 12:02:54   t13ex14
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI2543                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI2543                    *
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
      *    FÜR DIE FUNKTION DRKI2543
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  UPDATE <<<
      *
      ****************************************************************
      *
       01  PI-DRKI2543.
           05  PI-DRKI2543-KEY.
             10 PI-DRKI2543-GES                  PIC  9(002).
             10 PI-DRKI2543-VERTRAGSNUMMER       PIC  X(015).
             10 PI-DRKI2543-VERTRAGSID           PIC  X(013).
             10 PI-DRKI2543-PERSONENID           PIC  X(013).
             10 PI-DRKI2543-ADRESSEID            PIC  X(013).

           05  PI-DRKI2543-DATEN.
             10 PI-DRKI2543-ANZNAME              PIC  X(030).
             10 PI-DRKI2543-AEND-NNAME-KZ        PIC  X(001).
             10 PI-DRKI2543-ANZVORNAME           PIC  X(030).
             10 PI-DRKI2543-AEND-VNAME-KZ        PIC  X(001).
             10 PI-DRKI2543-GEBGRUENDDAT         PIC  X(010).
             10 PI-DRKI2543-AEND-GEBDAT-KZ       PIC  X(001).
             10 PI-DRKI2543-ANREDEID             PIC S9(004) COMP.
             10 PI-DRKI2543-AEND-ANREDE-KZ       PIC  X(001).
             10 PI-DRKI2543-GESCHL               PIC  X(001).
             10 PI-DRKI2543-AEND-GESCHL-KZ       PIC  X(001).
             10 PI-DRKI2543-GEANDERTPGM          PIC  X(008).
      *
       01  PO-DRKI2543.
           05  PO-DRKI2543-PERSONENID            PIC  X(013).
           05  PO-DRKI2543-RC                    PIC  X(003).
               88 PO-DRKI2543-RC-OK              VALUE 'OK '.
               88 PO-DRKI2543-RC-NOT-FOUND       VALUE 'NFD'.
               88 PO-DRKI2543-RC-NOK             VALUE 'NOK'.

      *    für Log-Datei des Hauptprogramms:
           05  PO-DRKI2543-FEHLERSATZ.
               10 PO-DRKI2543-FUNKTION           PIC  X(10).
               10 PO-DRKI2543-TABELLE            PIC  X(20).
               10 PO-DRKI2543-SQL-FEHLER         PIC S9(04).
               10 PO-DRKI2543-KEY                PIC  X(40).
               10 FILLER REDEFINES PO-DRKI2543-KEY.
                  15 PO-DRKI2543-GES             PIC  9(002).
                  15 PO-DRKI2543-VNR             PIC  X(015).
      *
      ******* ENDE COPY-MEMBER **************************************
