      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA1019.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\DRPA1019.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 14:29:58   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   12 Aug 1998 16:54:56   T13EX19
      *VB-SCHNITTSTELLE
      *
      *   Rev 1.0   14 Jul 1998 13:59:16   P13SP19
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRPA1019 FUNKTION ERM. ZULETZT GüLTIGEN BETR *
      *    --------------------------------------------------------- *
      *    ERSTELLT: Veit                                            *
      *    DATUM:    15.07.1998                                      *
      ****************************************************************
      *    VERSION   01                                              *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AKI00001!15.07.98! 01 ! NEUERSTELLUNG             ! Veit     *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRPA1019                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      *    >>>  ERMITTELN ZULETZT GüLTIGEN BETREUER  <<<             *
      *                                                              *
      ****************************************************************
      *
       01 PI-DRPA1019.
          05 PI-DRPA1019-KEY.
             10 PI-DRPA1019-BVH-UNTST-NR        PIC  S9(009) COMP.
      *
       01 PO-DRPA1019.
          05 PO-DRPA1019-RC                     PIC  X(003).
             88 PO-DRPA1019-RC-OK                     VALUE 'OK '.
             88 PO-DRPA1019-RC-SDNR-KEIN-BETR         VALUE 'SKB'.
          05 PO-DRPA1019-BVHDATEN.
             10 PO-DRPA1019-BVH-BETR-NR         PIC  S9(009) COMP.
             10 PO-DRPA1019-BVH-BETR-TYP        PIC  S9(009) COMP.
      *
      ****** ENDE COPY-MEMBER  DRPA1019 ******************************
