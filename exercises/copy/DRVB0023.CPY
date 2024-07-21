      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB0023.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\DRVB0023.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 15:57:46   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.0   Nov 04 1995 09:07:44   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ***************************************************************** 00010000
      *    COPY-MEMBER: ÜBERGABEBEREICH  (DRVB0023)                   * 00020000
      *                 ERMITTELN BETREUERNAME, STUFE                 * 00030000
      *    --------------------------------------------------------   * 00030000
      *    VERSION   01                                               * 00040000
      *    --------------------------------------------------------   * 00050000
      *    ERSTELLT: M. GIESE                                         * 00060000
      *    DATUM:    24.04.1993                                       * 00070000
      *    STAND:    24.04.1993                                       * 00080000
      ***************************************************************** 00090000

       01   PI-UPVB0032.
         05  PI-UPVB0032-MA-ST-NR      PIC S9(09) COMP.
         05  PI-UPVB0032-MA-MAT-DAT-BIS   PIC  X(10).

       01   PO-UPVB0032.
         05  PO-UPVB0032-RETCODE            PIC  X(01).
         05  PO-UPVB0032-BETR-NAME          PIC  X(40).
         05  PO-UPVB0032-BETR-STF           PIC  X(03).
         05  PO-UPVB0032-BETR-PRV-STZ       PIC  X(03).
         05  PO-UPVB0032-BETR-PRZ           PIC  X(01).
         05  PO-UPVB0032-BETR-TYP           PIC S9(04).
         05  PO-UPVB0032-BETR-MSBZ-DAT-VON  PIC  X(26).
         05  PO-UPVB0032-TUPEL-BVH          PIC  X(96).
         05  PO-UPVB0032-TUPEL-EDD          PIC  X(88).

                                                                        01590000
      ******** ENDE COPY-MEMBER ÜBERGABEBEREICH        (DRVB0023) ***** 01600000
