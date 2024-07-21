      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  27.11.2003
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2003 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI1651.CPY  $
      *    $Revision:   2.3  $
      *
      *    $Log:   L:/mf/CPY/DRKI1651.CPv  $
      *
      *   Rev 2.3   Sep 18 2012 18:21:30   t13ex27
      *AKI00886: Host - neues Design Kundenaktualisierung-Kundensta
      *tus und Anzahl Betreuer bzw. NEUKUNDE
      *
      *   Rev 2.2   Apr 07 2004 13:44:32   t13ex39
      *Neues Kennzeichen 'L' in KUNDENINTVBBEZ
      *
      *   Rev 2.1   Apr 07 2004 13:24:50   t13ex39
      *Neues Kennzeichen 'L' in KUNDENINTVBBEZ
      *
      *   Rev 2.0   Nov 27 2003 13:57:00   T13EX39
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRKI1651                                     *
      *    --------------------------------------------------------  *
      *    SCHNITTSTELLE FüR DAS UPRO UPKI0168                       *
      *    MIT DEN DATEN, DIE EINMAL PRO UPRO ÜBERGEBEN WERDEN       *
      *    --------------------------------------------------------  *
      ****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AKI00282!01.03.04! 1.0! NEU                       ! HORNUNG  *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER
      *    FÜR DIE FUNKTION DRKI1651
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *
      ****************************************************************
      *
       01  PI-DRKI1651.
           05  PI-DRKI1651-DATEN.
             10 PI-DRKI1651-PERSONENID         PIC X(13).

       01  PO-DRKI1651.
           05  PO-DRKI1651-RC                 PIC  X(003).
               88 PO-DRKI1651-RC-OK                        VALUE 'OK '.
               88 PO-DRKI1651-RC-NOT-FOUND                 VALUE 'NFD'.
      *
      *
      ******* ENDE COPY-MEMBER **************************************

