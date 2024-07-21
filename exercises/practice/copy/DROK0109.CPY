      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  14.01.2010
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2010 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DROK0109.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DROK0109.CPv  $
      *
      *   Rev 2.2   Jan 25 2010 17:13:28   t13ex51
      *AKI00899: Reaktivierung postalische Prüfung
      *
      *   Rev 2.1   Jan 19 2010 18:44:28   t13ex51
      *AKI00899: Reaktivierung postalische Prüfung
      *
      *   Rev 2.0   Jan 14 2010 13:12:40   t13ex51
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DROK0109                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DROK0109                    *
      *    --------------------------------------------------------   *
      ****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AKI00899!14.01.10! 2.1! NEU                       !HRO       *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER
      *    FÜR DIE FUNKTION DROK0109
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  UPDATE  <<<
      *
      ****************************************************************
      *
       01  PI-DROK0109.
           05  PI-DROK0109-KEY.

             10 PI-DROK0109-HADTICKETID     PIC S9(18) COMP-3.

           05  PI-DROK0109-DATEN.

             10 PI-DROK0109-HADADRESSEALTID   PIC X(13).
             10 PI-DROK0109-HADLFDNR          PIC S9(4) COMP.

      *
       01  PO-DROK0109.
           05  PO-DROK0109-RC                 PIC  X(003).
               88 PO-DROK0109-RC-OK                        VALUE 'OK '.
               88 PO-DROK0109-RC-NOT-FOUND                 VALUE 'NFD'.
      *
      ******* ENDE COPY-MEMBER ***************************************

