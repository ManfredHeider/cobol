      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB0334.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\DRVB0334.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 15:58:56   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   15 Dec 1998 10:53:48   T13AE42
      *AAS00079 - Neuerstellung wg. Delta-Rueckkonvertierung
      *
      *   Rev 1.0   27 Oct 1998 16:34:54   P13SP19
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRVB0334                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DAS UPRO UPVB0105                        *
      *    --------------------------------------------------------   *
      *    ERSTELLT: WALB                                            *
      *    DATUM:    04.11.1998                                       *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AAS00079!04.11.98! 01 ! NEU                       ! WALB     *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER
      *    FÜR DIE FUNKTION DRVB0334
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *
      ****************************************************************
      *
       01  PI-DRVB0334.
           05  PI-DRVB0334-DL-VBNR            PIC  S9(009) COMP.
      *
       01  PO-DRVB0334.
           05  PO-DRVB0334-RC                 PIC  X(003).
               88 PO-DRVB0334-RC-OK                        VALUE 'OK '.
               88 PO-DRVB0334-RC-NOK                       VALUE 'NOK'.
      *
      *
      ******* ENDE COPY-MEMBER **************************************
