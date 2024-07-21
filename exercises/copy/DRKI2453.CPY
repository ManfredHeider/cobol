      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  21.04.2011
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2011 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI2453.CPY  $
      *    $Revision:   2.4  $
      *
      *    $Log:   L:/mf/CPY/DRKI2453.CPv  $
      *
      *   Rev 2.4   Sep 20 2012 09:56:38   t13ae49
      *AKI00886: NKDA: Upros VN / BZ / VP insert, update, delete
      *
      *   Rev 2.3   Sep 06 2012 14:33:20   t13ae49
      *AKI00886: NKDA: Upros VN / BZ / VP insert, update, delete
      *
      *   Rev 2.2   Sep 06 2012 11:22:22   t13ex14
      *AKI00886: NKDA: Upros VN / BZ / VP insert, update, delete
      *
      *   Rev 2.1   Sep 06 2012 11:09:38   t13ex14
      *AKI00886: NKDA: Upros VN / BZ / VP insert, update, delete
      *
      *   Rev 2.0   Apr 21 2011 07:55:12   t13ex14
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI2453                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI2453                    *
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
      *    FÜR DIE FUNKTION DRKI2453
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  UPDATE <<<
      *
      ****************************************************************
      *
       01  PI-DRKI2453.
           05  PI-DRKI2453-KEY.
             10 PI-DRKI2453-GES                  PIC  9(002).
             10 PI-DRKI2453-VERTRAGSNUMMER       PIC  X(015).
             10 PI-DRKI2453-VERTRAGSID           PIC  X(013).
             10 PI-DRKI2453-PERSONENID           PIC  X(013).
             10 PI-DRKI2453-ADRESSEID            PIC  X(013).

           05  PI-DRKI2453-DATEN.
             10 PI-DRKI2453-ROLLE-VERTRAG        PIC  X(001).
             10 PI-DRKI2453-ROLLE-TARIF          PIC  X(001).
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
      *      ändern KUNDE:
             10 PI-DRKI2453-ANZNAME              PIC  X(030).
             10 PI-DRKI2453-AEND-NNAME-KZ        PIC  X(001).
             10 PI-DRKI2453-ANZVORNAME           PIC  X(030).
             10 PI-DRKI2453-AEND-VNAME-KZ        PIC  X(001).
             10 PI-DRKI2453-ANREDEID             PIC S9(004) COMP.
             10 PI-DRKI2453-AEND-ANREDE-KZ       PIC  X(001).
             10 PI-DRKI2453-GESCHL               PIC  X(001).
             10 PI-DRKI2453-AEND-GESCHL-KZ       PIC  X(001).
             10 PI-DRKI2453-GEBDAT               PIC  X(010).
             10 PI-DRKI2453-AEND-GEBDAT-KZ       PIC  X(001).
             10 PI-DRKI2453-GEANDERTPGM          PIC  X(008).
             10 PI-DRKI2453-KND-AENDERUNG        PIC  X(001).
      *         N = keine, J = Änderung KUNDE

      *      nur AML/LEBENPERSONEIGEN:
             10 PI-DRKI2453-VTRBEZ-ARTKZ         PIC  X(001).
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
             10 PI-DRKI2453-PERSONLFDNR          PIC  9(002).
      *
       01  PO-DRKI2453.
      *    Daten
      *    Rollen für LEBENPERSONEIGEN
           05  PO-DRKI2453-ROLLE-LPE             PIC  X(001).
               88 PO-DRKI2453-ROLLE-H            VALUE 'H'.
      *           'H'auptversichete Person
               88 PO-DRKI2453-ROLLE-M            VALUE 'M'.
      *           'M'itversicherte Person
               88 PO-DRKI2453-ROLLE-2M           VALUE '2'.
      *           '2'. mitversicherte Person
               88 PO-DRKI2453-ROLLE-BZ           VALUE 'Z'.
      *           Person ist Beitrags'Z'ahler
      *
      *    Rollen für KRANKENPERSEIGEN
           05  PO-DRKI2453-ROLLE-KPE             PIC  X(002).
               88 PO-DRKI2453-ROLLE-NP           VALUE 'NP'.
      *           Person ist VN u. VP
               88 PO-DRKI2453-ROLLE-N            VALUE 'N '.
      *           Person ist nur VN
               88 PO-DRKI2453-ROLLE-P            VALUE ' P'.
      *           Person ist nur VP
      *
      *    Returncode
           05  PO-DRKI2453-RC                    PIC  X(003).
               88 PO-DRKI2453-RC-OK              VALUE 'OK '.
               88 PO-DRKI2453-RC-NOT-FOUND       VALUE 'NFD'.
               88 PO-DRKI2453-RC-NOK             VALUE 'NOK'.

      *    für Log-Datei des Hauptprogramms:
           05  PO-DRKI2453-FEHLERSATZ.
               10 PO-DRKI2453-FUNKTION           PIC  X(10).
               10 PO-DRKI2453-TABELLE            PIC  X(20).
               10 PO-DRKI2453-SQL-FEHLER         PIC S9(04).
               10 PO-DRKI2453-KEY                PIC  X(40).
               10 FILLER REDEFINES PO-DRKI2453-KEY.
                  15 PO-DRKI2453-GES             PIC  9(002).
                  15 PO-DRKI2453-VNR             PIC  X(015).
      *
      ******* ENDE COPY-MEMBER **************************************
