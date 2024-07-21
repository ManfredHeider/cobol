      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  05.03.2007
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2007 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI2027.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRKI2027.CPv  $
      *
      *   Rev 2.1   20 Mar 2007 09:13:22   t13ex27
      *FKI00979: DB-Bereinigung für Online-KI DupKeys in VERSTARIFR
      *OLLE
      *
      *   Rev 2.0   Mar 05 2007 17:24:34   t13ex27
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI2027                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI2027                    *
      *    --------------------------------------------------------   *
      ****************************************************************
      *   erzeugt mit genOZP - (c) PMSmicado  07.03.2007/11:17:33
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
      *    FÜR DIE FUNKTION DRKI2027
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  INSERT  <<<
      *
      ****************************************************************
      *
       01  PI-DRKI2027.
           05  PI-DRKI2027-KEY.


             10 PI-DRKI2027-KRTVERSTARLFDNR   PIC S9(4) COMP.
             10 PI-DRKI2027-KRTVERTRAGSID   PIC X(13).
             

           05  PI-DRKI2027-DATEN.


             10 PI-DRKI2027-KRTBEITRAGPRIVAT   PIC S9(9)V9(2) COMP-3.
             10 PI-DRKI2027-KRTBEITRAGVERZ   PIC S9(2)V9(1) COMP-3.
             10 PI-DRKI2027-KRTBEITRFRWILLWILL   PIC S9(9)V9(2) COMP-3.
             10 PI-DRKI2027-KRTGESCHAEFTSST   PIC X(20).
             10 PI-DRKI2027-KRTGESCHAEFTSSTID   PIC S9(4) COMP.
             10 PI-DRKI2027-KRTPERSONENNUMMER   PIC S9(4) COMP.
             
      *
       01  PO-DRKI2027.
           05  PO-DRKI2027-RC                 PIC  X(003).
               88 PO-DRKI2027-RC-OK                        VALUE 'OK '.
               88 PO-DRKI2027-RC-DUPLICATE-VALUE           VALUE 'DVA'.
      *
      ******* ENDE COPY-MEMBER **************************************
