      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRAT0200.CPY  $
      *    $Revision:   1.1  $
      *
      *    $Log:   L:\MF\CPY\DRAT0200.CPv  $
      *
      *   Rev 1.1   Jul 28 1999 13:35:00   T13EX16
      * 
      *
      *   Rev 1.0   05 Sep 1997 09:22:46   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRAT0200                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRAT0200 IM ZWAT0021        *
      *    --------------------------------------------------------   *
      *    ERSTELLT: BOHLINGER                                        *
      *    DATUM:    27.11.1997                                       *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * APA00012!27.11.97!    ! NEU                       ! BOHLINGER*
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER
      *    FÜR DIE FUNKTION DRAT0200: PRüFEN KONDITION-VORHANDEN
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      ****************************************************************
      *
       01  PI-DRAT0200.
           05  PI-DRAT0200-DATEN.
               10 PI-DRAT0200-NTZ-ID         PIC S9(015) PACKED-DECIMAL.
      *
       01  PO-DRAT0200.
           05  PO-DRAT0200-RC                PIC  X(003).
               88 PO-DRAT0200-RC-OK                      VALUE 'OK '.
               88 PO-DRAT0200-RC-NOT-FOUND               VALUE 'NFD'.
      *
