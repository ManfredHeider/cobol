      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  06.04.2006
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2006 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DROK0019.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DROK0019.CPv  $
      *
      *   Rev 2.2   27 Mar 2007 10:18:56   t13ex27
      *AKI00557: Ticketverfahren HOST-DB <-> Online-DB - OZPs
      *
      *   Rev 2.1   Feb 27 2007 12:56:08   t13ex27
      *AKI00557: Ticketverfahren HOST-DB <-> Online-DB - OZPs
      *
      *   Rev 2.0   Apr 06 2006 15:40:48   t13ex27
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRoK0019                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRoK0019                    *
      *    --------------------------------------------------------   *
      ****************************************************************
      *   erzeugt mit genOZP - (c) PMSmicado  25.04.2006/14:14:28
      ****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AKI00557!10.03.06! 2.1! NEU                       !I. Frenzel*
      * -------------------------------------------------------------*
      * AKI00557!01.02.07!    ! Anpassung an geänderte    ! HSE      *
      *         !        !    ! Tabellenstruktur          !          *
      * -------------------------------------------------------------*
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER
      *    FÜR DIE FUNKTION DRoK0019
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  INSERT  <<<
      *
      ****************************************************************
      *
       01  PI-DROK0019.
           05  PI-DROK0019-KEY.
020107       10 PI-DROK0019-HADTICKETID      PIC 9(18).

           05  PI-DROK0019-DATEN.
             10 PI-DROK0019-HADADRESSEALTID   PIC X(13).
             10 PI-DROK0019-HADLFDNR   PIC S9(4) COMP.
             10 PI-DROK0019-HADPERSONENID   PIC X(13).
             10 PI-DROK0019-HADVERTRAGSID   PIC X(13).
             10 PI-DROK0019-HADAENDGRUNDID   PIC S9(4) COMP.
020107*      10 PI-DROK0019-HADBDSLDID   PIC X(3).
             10 PI-DROK0019-HADLANDID   PIC X(3).
020107*      10 PI-DROK0019-HADOKIERSTELLT   PIC X(26).
             10 PI-DROK0019-HADORT   PIC X(30).
             10 PI-DROK0019-HADORTSTEIL   PIC X(30).
             10 PI-DROK0019-HADPLZ   PIC X(15).
             10 PI-DROK0019-HADSTATUS   PIC S9(4) COMP.
             10 PI-DROK0019-HADSTRHAUSNUM   PIC X(45).
             10 PI-DROK0019-HADVBID   PIC X(13).

      *
       01  PO-DROK0019.
           05  PO-DROK0019-RC                 PIC  X(003).
               88 PO-DROK0019-RC-OK                        VALUE 'OK '.
               88 PO-DROK0019-RC-DUPLICATE-VALUE           VALUE 'DVA'.
      *
      ******* ENDE COPY-MEMBER **************************************
