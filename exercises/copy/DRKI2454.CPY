      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  21.04.2011
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2011 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI2454.CPY  $
      *    $Revision:   2.4  $
      *
      *    $Log:   L:/mf/CPY/DRKI2454.CPv  $
      *
      *   Rev 2.4   Sep 20 2012 09:56:44   t13ae49
      *AKI00886: NKDA: Upros VN / BZ / VP insert, update, delete
      *
      *   Rev 2.3   Sep 06 2012 14:33:26   t13ae49
      *AKI00886: NKDA: Upros VN / BZ / VP insert, update, delete
      *
      *   Rev 2.2   Sep 06 2012 11:22:30   t13ex14
      *AKI00886: NKDA: Upros VN / BZ / VP insert, update, delete
      *
      *   Rev 2.1   Sep 06 2012 11:09:48   t13ex14
      *AKI00886: NKDA: Upros VN / BZ / VP insert, update, delete
      *
      *   Rev 2.0   Apr 21 2011 07:55:18   t13ex14
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI2454                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI2454                    *
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
      *    FÜR DIE FUNKTION DRKI2454
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  DELETE <<<
      *
      ****************************************************************
      *
       01  PI-DRKI2454.
           05  PI-DRKI2454-KEY.
             10 PI-DRKI2454-GES                  PIC  9(002).
             10 PI-DRKI2454-VERTRAGSID           PIC  X(013).
             10 PI-DRKI2454-VERTRAGSNUMMER       PIC  X(015).
             10 PI-DRKI2454-PERSONENID           PIC  X(013).
             10 PI-DRKI2454-VBPERSID             PIC  X(015).

           05  PI-DRKI2454-DATEN.
             10 PI-DRKI2454-ROLLE-VERTRAG        PIC  X(001).
             10 PI-DRKI2454-ROLLE-TARIF          PIC  X(001).
      *         ROLLE-VERTRAG = 'N' und ROLLE-TARIF = SPACE,
      *           Person ist nur VN --> UPKI0224 (VN-Ins/Del)
      *
      *         ROLLE-VERTRAG = 'N' und ROLLE-TARIF = 'P'
      *           Person ist VN u. VP --> UPKI0224 (VN-Ins/Del).
      *                           und --> UPKI0219 (KRANKENPERSEIGEN)
      *         ROLLE-VERTRAG = SPACE und ROLLE-TARIF = 'P'
      *           Person ist nur VP --> AML/UPKI0218 (LEBENPERSONEIGEN)
      *                        oder --> CKV/UPKI0219 (KRANKENPERSEIGEN)
      *
             10 PI-DRKI2454-ANZNAME              PIC  X(030).
             10 PI-DRKI2454-ANZVORNAME           PIC  X(030).
             10 PI-DRKI2454-VERTRAGSAENDER       PIC  X(010).
             10 PI-DRKI2454-BESTANDFKT-KZ        PIC  X(001).
                88 S-BESTANDFKT-NEIN             VALUE ' ', 'N'.
                88 S-BESTANDFKT-JA               VALUE 'J'.

      *      nur AML/LEBENPERSONEIGEN:
             10 PI-DRKI2454-VTRBEZ-ARTKZ         PIC  X(001).
      *         ROLLE-VERTRAG = 'Z'
      *           Person ist Beitrags'Z'ahler
      *         ROLLE-VERTRAG = SPACE und ROLLE-TARIF = 'P'
      *           VTRBEZ-ARTKZ = SPACE
      *             'H'auptversichete Person
      *           VTRBEZ-ARTKZ = 'M'
      *             'M'itversicherte Person
      *           VTRBEZ-ARTKZ = '2'
      *             '2'. mitversicherte Person
      *
      *      nur CKV/KRANKENPERSEIGEN:
             10 PI-DRKI2454-PERSONLFDNR          PIC  9(002).

      *
       01  PO-DRKI2454.
      *    Daten
      *    Rollen für LEBENPERSONEIGEN
           05  PO-DRKI2454-ROLLE-LPE             PIC  X(001).
               88 PO-DRKI2454-ROLLE-H            VALUE 'H'.
      *           'H'auptversichete Person
               88 PO-DRKI2454-ROLLE-M            VALUE 'M'.
      *           'M'itversicherte Person
               88 PO-DRKI2454-ROLLE-2M           VALUE '2'.
      *           '2'. mitversicherte Person
               88 PO-DRKI2454-ROLLE-BZ           VALUE 'Z'.
      *           Person ist Beitrags'Z'ahler
      *
      *    Rollen für KRANKENPERSEIGEN
           05  PO-DRKI2454-ROLLE-KPE             PIC  X(002).
               88 PO-DRKI2454-ROLLE-NP           VALUE 'NP'.
      *           Person ist VN u. VP
               88 PO-DRKI2454-ROLLE-N            VALUE 'N '.
      *           Person ist nur VN
               88 PO-DRKI2454-ROLLE-P            VALUE ' P'.
      *           Person ist nur VP
      *
      *    Returncode
           05  PO-DRKI2454-RC                    PIC  X(003).
               88 PO-DRKI2454-RC-OK              VALUE 'OK '.
               88 PO-DRKI2454-RC-NOT-FOUND       VALUE 'NFD'.
               88 PO-DRKI2454-RC-NOK             VALUE 'NOK'.

      *    für Log-Datei des Hauptprogramms:
           05  PO-DRKI2454-FEHLERSATZ.
               10 PO-DRKI2454-FUNKTION           PIC  X(10).
               10 PO-DRKI2454-TABELLE            PIC  X(20).
               10 PO-DRKI2454-SQL-FEHLER         PIC S9(04).
               10 PO-DRKI2454-KEY                PIC  X(40).
               10 FILLER REDEFINES PO-DRKI2454-KEY.
                  15 PO-DRKI2454-GES             PIC  9(002).
                  15 PO-DRKI2454-VNR             PIC  X(015).
      *
      ******* ENDE COPY-MEMBER **************************************
