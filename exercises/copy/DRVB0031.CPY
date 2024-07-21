      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB0031.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\DRVB0031.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 15:57:48   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.0   Nov 04 1995 09:09:04   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ***************************************************************** 00010008
      *    COPY-MEMBER: DRVB0031                                      * 00020008
      *    --------------------------------------------------------   * 00030008
      *    SCHNITTSTELLE UPVB0033 ERMITTELN BETREUER                  * 00040008
      *    --------------------------------------------------------   * 00041008
      *    ERSTELLT: GIESE                                            * 00042008
      *    VERSION:  01                                               * 00042109
      *    DATUM:    15.04.1993                                       * 00043008
      *    STAND:    03.05.1993                                       * 00044008
      ***************************************************************** 00045008
      *                                                                 00046008
       01  PI-UPVB0033.                                                 00047008
           05 PI-UPVB0033-MA-ST-NR      PIC S9(09) COMP.                00048008
      *                                                                 00049008
       01  PO-UPVB0033.                                                 00049108
           05 PO-UPVB0033-RC-MELD-TYP        PIC  X(01).                00049208
           05 PO-UPVB0033-RC-MELD-NR         PIC S9(04) COMP.           00049308
           05 PO-UPVB0033-BETR-ST-NR-X       PIC  X(07).                00049408
           05 PO-UPVB0033-BETR-ST-NR         PIC S9(09) COMP.           00049508
           05 PO-UPVB0033-BETR-ST-PRZ        PIC  X(01).                00049608
           05 PO-UPVB0033-BETR-MA-TYP        PIC S9(04) COMP.           00049708
           05 PO-UPVB0033-BETR-ST-TYP        PIC S9(04) COMP.           00049808
           05 PO-UPVB0033-BETREUUNG-DAT-VON  PIC  X(26).                00049908
           05 PO-UPVB0033-TUPEL-BVH          PIC  X(96).                00050008
           05 PO-UPVB0033-TUPEL-EDD          PIC  X(88).                00050108
      *                                                                 00050208
      ******** ENDE COPY-MEMBER (DRVB0031) *******************          00050308
