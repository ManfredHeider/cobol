      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  04.04.2007
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2007 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DROK0083.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DROK0083.CPv  $
      *
      *   Rev 2.2   05 Apr 2007 17:13:26   t13ex27
      *AKI00557: Ticketverfahren HOST-DB <-> Online-DB
      *
      *   Rev 2.1   05 Apr 2007 15:46:16   t13ex27
      *AKI00557: Ticketverfahren AD/ID und Kunde-VB-Beziehung und A
      *dressänderung
      *
      *   Rev 2.0   04 Apr 2007 12:42:18   t13ex27
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DROK0083                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DROK0083                    *
      *    --------------------------------------------------------   *
      ****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AKI00557!10.03.06! 2.1! NEU                       !I. Frenzel*
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER
      *    FÜR DIE FUNKTION DROK0083
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  UPDATE  <<<
      *
      ****************************************************************
      *
       01  PI-DROK0083.
           05  PI-DROK0083-KEY.


             10 PI-DROK0083-HADTICKETID     PIC S9(18) COMP-3.


           05  PI-DROK0083-DATEN.


             10 PI-DROK0083-HADPERSONENID   PIC X(13).


      *
       01  PO-DROK0083.
           05  PO-DROK0083-RC                 PIC  X(003).
               88 PO-DROK0083-RC-OK                        VALUE 'OK '.
               88 PO-DROK0083-RC-NOT-FOUND                 VALUE 'NFD'.
      *
      ******* ENDE COPY-MEMBER ***************************************
