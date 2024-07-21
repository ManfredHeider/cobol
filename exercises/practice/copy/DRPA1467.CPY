      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Januar 2001
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 2001 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA1467.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRPA1467.CPv  $
      *
      *   Rev 2.2   Jul 04 2011 14:55:50   T13EX44
      *APA01228: SEB Immo (ProdPart 47): befristete Sonderprovision
      *zahlung
      *
      *   Rev 2.1   07 Feb 2001 14:03:56   T13EX22
      *APA00118: Neue Funktion Pin prüfen für BfG
      *
      *   Rev 2.0   09 Jan 2001 10:12:18   p13sp16
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRPA1467 FUNKTION PIN FÜR BFG PRÜFEN         *
      *                                                              *
      *    -------------------------------------------------------   *
      *    ERSTELLT: MUND                                            *
      *    DATUM:    07.02.2001                                      *
      ****************************************************************
      *    VERSION   01                                              *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * APA00118!07.02.01!2.1 ! NEU                       ! MUND     *
      * APA01228!04.07.11!2.2 ! VNR DAZU                  ! SCHEIDEL *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRPA1467                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      ****************************************************************
       01 PI-DRPA1467.
             10 PI-DRPA1467-ZVP-VERSION     PIC S9(004) BINARY.
             10 PI-DRPA1467-PGEB-GES        PIC S9(004) BINARY.
             10 PI-DRPA1467-PIN             PIC S9(009) BINARY.
             10 PI-DRPA1467-VERTR-BEGINN    PIC  X(010).
040711       10 PI-DRPA1467-VNR             PIC  X(015).

       01 PO-DRPA1467.
          05 PO-DRPA1467-RC                 PIC  X(003).
             88 PO-DRPA1467-RC-OK                VALUE 'OK '.
             88 PO-DRPA1467-RC-VSTAT-GEFUELLT    VALUE 'VSG'.
      ******** ENDE COPY-MEMBER **************************************

