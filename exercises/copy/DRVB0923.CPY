      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  04.11.2008
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2008 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB0923.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRVB0923.CPv  $
      *
      *   Rev 2.1   Nov 18 2008 14:41:06   t13ae36
      *AVB01012: Dublettenmeldung
      *
      *   Rev 2.0   Nov 04 2008 13:14:24   t13ae36
      *Archiverstellung
      *
      *****************************************************************
      *    COPY-MEMBER: DRVB0923                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRVB0923                    *
      *    --------------------------------------------------------   *
      *    ERSTELLT: Franke                                           *
      *    DATUM:    06.11.2008                                       *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AVB01012!06.11.08! 01 ! NEU                       ! Franke   *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER
      *    FÜR DIE FUNKTION DRVB0923
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  SELECT ...                <<<
      *
      *
      *LIEFERT:
      *PO-DRVB0923-MAN-BEZUGS-NR   =  1234567
      *
      ****************************************************************
      *
       01  PI-DRVB0923.
           05  PI-DRVB0923-MAN.
               10 PI-DRVB0923-KNR-ANREDE          PIC S9(004) COMP.
               10 PI-DRVB0923-MAN-VORNAME         PIC  X(020).
               10 PI-DRVB0923-MAN-NAME            PIC  X(040).
               10 PI-DRVB0923-MAN-GEBURTSDAT      PIC  X(010).

       01  PO-DRVB0923.
           05  PO-DRVB0923-RC                     PIC  X(003).
               88 PO-DRVB0923-RC-OK                    VALUE 'OK '.
               88 PO-DRVB0923-RC-NOT-FOUND             VALUE 'NFD'.
               88 PO-DRVB0923-RC-MULT-ROWS             VALUE 'MUR'.
      *
           05  PO-DRVB0923-MAN-BEZUGS-NR          PIC S9(007) BINARY.
      *
      ******* ENDE COPY-MEMBER **************************************
      *---------------------------------------------------------------*
