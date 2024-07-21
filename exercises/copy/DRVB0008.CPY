      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB0008.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\DRVB0008.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 15:57:42   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   Feb 20 1997 17:01:16   P13PVCS
      *Neuanlage des Archivs, weil es versehentlich gelöscht worden war.
      *
      *   Rev 1.0   Feb 19 1997 09:33:32   P13PVCS
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRVB0008                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE UPVB0017 PRÜFEN BESCHRÄNKUNGEN, OB           *
      *                           BEFÖRDERUNGSHINDERNIS               *
      *    --------------------------------------------------------   *
      *    VERSION   01                                               *
      *    --------------------------------------------------------   *
      *    ERSTELLT: GIESE                                            *
      *    DATUM:    24.04.1993                                       *
      *    STAND:    24.04.1993                                       *
      *****************************************************************
      *
       01  PI-UPVB0017.
           05 PI-UPVB0017-MA-ST-NR       PIC S9(09) COMP.
           05 PI-UPVB0017-MS-NR          PIC S9(04) COMP.
      *    05 PI-UPVB0017-BS-NR          PIC S9(04) COMP.
      *    SOLL DIE HÖHERSTUFUNG GECHECKT WERDEN J/N?
           05 PI-UPVB0017-MS-HOCH-CHECK  PIC  X(01).
      *    SOLL DIE RÜCKSTUFUNG GECHECKT WERDEN J/N?
           05 PI-UPVB0017-MS-RUECK-CHECK PIC  X(01).
      *    SOLL BONUS   GECHECKT WERDEN J/N?
           05 PI-UPVB0017-BS-CHECK       PIC  X(01).
      *    BETREUERWECHSEL              J/N?
           05 PI-UPVB0017-BETR-WECHSEL   PIC  X(01).
      *    BETREUERWECHSEL              J/N?
           05 PI-UPVB0017-VBP-CHECK      PIC  X(01).
      *    BESCHRÄNKUNG FÜR BETREUER    J/N?
           05 PI-UPVB0017-BETR-WV-CHECK  PIC  X(01).
      *    BESCHRÄNKUNG VORHANDEN       J/N?
           05 PI-UPVB0017-BES-CHECK      PIC  X(01).
      *
       01  PO-UPVB0017.
           05 PO-UPVB0017-RC-BES         PIC X(01).
           05 PO-UPVB0017-RC-BES-HINW    PIC X(01).
      *
      ******** ENDE COPY-MEMBER (DRVB0008) *******************
