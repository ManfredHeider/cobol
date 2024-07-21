      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  02.11.2006
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2006 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRAN0164.CPY  $
      *    $Revision:   2.3  $
      *
      *    $Log:   L:/mf/CPY/DRAN0164.CPv  $
      *
      *   Rev 2.3   Jun 26 2014 08:15:20   t13ae34
      *AAN01168: VERMÖGENS- & EINKOMMENSPLAN - VEP 
      *
      *   Rev 2.2   08 Nov 2006 14:04:14   t13ae34
      *AAN00655: Erweiterung des PS auf 3 Stellen
      *
      *   Rev 2.1   06 Nov 2006 16:31:54   t13ae23
      *AAN00655: Erweiterung des PS auf 3 Stellen
      *
      *   Rev 2.0   02 Nov 2006 13:21:56   t13ae23
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRAN0164  FUNKTIONEN ZAUU0301                *
      *                           VERARBEITEN ANTRAG TDS-SÄTZE       *
      *                                                              *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AAN00655!02.11.06! 2.1! NEUERSTELLUNG             ! RAHNER   *
      * AAN00655!07.11.06! 2.2! EINE FUNKTION FÜR BAT/CICS! HEIDER   *
      * AAN01168!24.06.14! 2.3! COPY-MEMBER: DRAN0164     ! HEIDER   *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
       01  PI-ZAUU0301.
           03 PI-ZAUU0301-FUNKTION            PIC X(008).
              88 PI-ZAUU0301-DRAN0165             VALUE 'DRAN0165'.
