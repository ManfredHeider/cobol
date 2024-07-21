      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  06.11.2013
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2013 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRAT0520.CPY  $
      *    $Revision:   2.3  $
      *
      *    $Log:   L:/mf/CPY/DRAT0520.CPv  $
      *
      *   Rev 2.3   Jan 07 2014 13:05:34   t13ex78
      *AKI01259: Kontrolle tgl. Aktualisierungsläufe über ABLAUFKON
      *TROLLE
      *
      *   Rev 2.2   Jan 07 2014 08:47:54   t13ex78
      *AKI01259: Kontrolle tgl. Aktualisierungsläufe ü
      *ber ABLAUFK
      *ONTROLLE - Programme
      *
      *   Rev 2.1   Jan 07 2014 08:26:42   t13ex78
      *AKI01259: Kontrolle tgl. Aktualisierungsläufe über ABLAUFKON
      *TROLLE
      *
      *   Rev 2.0   Nov 06 2013 13:23:22   t13ex78
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRAT0520                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRAT0520                    *
      *    --------------------------------------------------------   *
      ****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AKI01259!06.11.13!    ! Neu                       ! TFL      *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER
      *    FÜR DIE FUNKTION DRAT0520
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  SELECT  <<<
      *
      ****************************************************************
      *
       01  PI-DRAT0520.
           05  PI-DRAT0520-KEY.


             10 PI-DRAT0520-ABKJOBNAME        PIC X(8).

       01  PO-DRAT0520.
           05  PO-DRAT0520-RC                 PIC  X(003).
               88 PO-DRAT0520-RC-OK           VALUE 'OK '.
               88 PO-DRAT0520-RC-NOT-FOUND    VALUE 'NFD'.

      *    05  PO-DRAT0520-DATEN.
      *
      *
      *      10 PO-DRAT0520-ABKLFDNR          PIC S9(9) COMP.
      *      10 PO-DRAT0520-ABKBEGABLAUF      PIC X(26).
      *      10 PO-DRAT0520-ABKBEGAKTU        PIC X(26).
      *      10 PO-DRAT0520-ABKBEGEINARB      PIC X(26).
      *      10 PO-DRAT0520-ABKENDAKTU        PIC X(26).
      *      10 PO-DRAT0520-ABKENDEINARB      PIC X(26).
      *      10 PO-DRAT0520-ABKKATALOGDATUM   PIC X(10).
      *      10 PO-DRAT0520-ABKPARTNERDATUM   PIC X(10).
      *      10 PO-DRAT0520-ABKERSTELLTTST    PIC X(26).



      ******* ENDE COPY-MEMBER **************************************
      **
