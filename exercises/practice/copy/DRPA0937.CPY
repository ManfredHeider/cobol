      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   drpa0937.cpy  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:\MF\CPY\drpa0937.cpv  $
      *
      *   Rev 2.1   May 04 1999 16:05:12   T01JM2S
      *APA00012 - NEUE FUNKTION DRPA1210
      *
      *   Rev 2.0   Feb 27 1999 14:29:54   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   11 Nov 1998 15:48:56   T01JM2S
      *PEG-PRODUKTIONSüBERGABE
      *
      *   Rev 1.0   01 Apr 1998 14:16:12   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRPA0937                                      *
      *    --------------------------------------------------------   *
      *    FUNKTIONSAUFRUF <<ERMITTELN PEG/BATCH >>                   *
      *    --------------------------------------------------------   *
      *    ERSTELLT: WOLFF                                            *
      *    DATUM:    27.04.1998                                       *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AFPA0012!27.04.98! 01 ! NEU                       ! WOLFF    *
      *         !        !    !                           !          *
      * APA00012!16.04.99!2.1 ! RC-NICHT-BUCHEN RAUSGENOM-! WOLFF    *
      *         !        !    ! MEN                       !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRPA0937                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      ****************************************************************
      *
       01 PI-DRPA0937.
          05 PI-DRPA0937-DATEN.
             10 PI-DRPA0937-NPM-TEIL.
                COPY DRPA0940.
      *
       01 PO-DRPA0937.
          05 PO-DRPA0937-RC                         PIC  X(003).
             88 PO-DRPA0937-RC-OK                         VALUE 'OK '.
160499*      88 PO-DRPA0937-RC-NICHT-BUCHEN               VALUE 'NBU'.
          05 PO-DRPA0937-DATEN.
             10 PO-DRPA0937-KOFV-TEIL.
                COPY DRPA0847.
      *
      ******* ENDE COPY-MEMBER **************************************
