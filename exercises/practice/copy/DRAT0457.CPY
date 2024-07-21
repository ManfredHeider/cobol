      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  29.01.2009
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2009 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRAT0457.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRAT0457.CPv  $
      *
      *   Rev 2.2   Aug 14 2009 09:01:30   t13ex14
      *AKI00779: Änderung DTA- u. Ablaufkontrolle bzgl. Lfdnr.
      *
      *   Rev 2.1   Feb 24 2009 13:45:08   t13ex14
      *AKI00787: Maschinelle Info über Aktualisierungsjobs - OZP-Ob
      *jekte
      *
      *   Rev 2.0   Jan 29 2009 11:25:54   t13ex14
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRAT0457                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRAT0457                    *
      *    --------------------------------------------------------   *
      ****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AKI00787!29.01.09!    ! Neu                       ! WMI      *
      *         !        !    !                           !          *
      * AKI00779!04.08.09!    !ABKLFDNR in PI-DRAT0457-KEY! WMI      *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER
      *    FÜR DIE FUNKTION DRAT0457
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  SELECT  <<<
      *
      ****************************************************************
      *
       01  PI-DRAT0457.
           05  PI-DRAT0457-KEY.


             10 PI-DRAT0457-ABKJOBNAME     PIC X(8).
             10 PI-DRAT0457-ABKMANDANTNR   PIC S9(9) COMP.
040809       10 PI-DRAT0457-ABKLFDNR       PIC S9(9) COMP.
             10 PI-DRAT0457-ABKPARTGESID   PIC S9(4) COMP.

      *
       01  PO-DRAT0457.
           05  PO-DRAT0457-RC                 PIC  X(003).
               88 PO-DRAT0457-RC-OK                        VALUE 'OK '.
               88 PO-DRAT0457-RC-NOT-FOUND                 VALUE 'NFD'.
      *
           05  PO-DRAT0457-DATEN.


040809*      10 PO-DRAT0457-ABKLFDNR          PIC S9(9) COMP.
             10 PO-DRAT0457-ABKBEGABLAUF      PIC X(26).
             10 PO-DRAT0457-ABKBEGAKTU        PIC X(26).
             10 PO-DRAT0457-ABKBEGEINARB      PIC X(26).
             10 PO-DRAT0457-ABKENDAKTU        PIC X(26).
             10 PO-DRAT0457-ABKENDEINARB      PIC X(26).
             10 PO-DRAT0457-ABKKATALOGDATUM   PIC X(10).
             10 PO-DRAT0457-ABKPARTNERDATUM   PIC X(10).


      *
      ******* ENDE COPY-MEMBER **************************************
