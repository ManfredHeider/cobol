      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB0019.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\DRVB0019.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 15:57:44   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.0   Nov 04 1995 09:06:52   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ***************************************************************** 00010000
      *    COPY-MEMBER: DRVB0019                                      * 00020000
      *    --------------------------------------------------------   * 00030000
      *    SCHNITTSTELLE UPVB0028 PRÜFEN BANKVERBINDUNG               * 00040000
      *    --------------------------------------------------------   * 00041000
      *    VERSION   01                                               * 00042008
      *    --------------------------------------------------------   * 00042108
      *    ERSTELLT: GIESE                                            * 00042208
      *    DATUM:    24.04.1993                                       * 00043008
      *    STAND:    24.04.1993                                       * 00044008
      ***************************************************************** 00045000
      *                                                                 00046000
       01  PI-UPVB0028.                                                 00047000
           05 PI-UPVB0028-ASPERRE               PIC X(01).              00048001
           05 PI-UPVB0028-ESPERRE               PIC X(01).              00048101
           05 PI-UPVB0028-ABANK                 PIC X(01).              00048205
           05 PI-UPVB0028-EBANK                 PIC X(40).              00048305
           05 PI-UPVB0028-ABLZ                  PIC X(01).              00048401
           05 PI-UPVB0028-EBLZ                  PIC X(08).              00048505
           05 PI-UPVB0028-AKTONR                PIC X(01).              00048601
           05 PI-UPVB0028-EKTONR                PIC X(20).              00048701
           05 PI-UPVB0028-ABVINH                PIC X(01).              00048801
           05 PI-UPVB0028-EBVINH                PIC X(40).              00048903
           05 PI-UPVB0028-ABVVONT               PIC X(02).              00049006
      *                                                                 00049100
      *                                                                 00049200
       01  PO-UPVB0028.                                                 00049300
           05 PO-UPVB0028-MELD-TYP              PIC  X(01).             00049507
           05 PO-UPVB0028-MELD-NR               PIC  9(02).             00049607
           05 PO-UPVB0028-FELDNAME              PIC  X(07).             00049707
           05 PO-UPVB0028-ESPERRE-ATTR          PIC  X(01).             00049804
           05 PO-UPVB0028-EBANK-ATTR            PIC  X(01).             00049904
           05 PO-UPVB0028-EBLZ-ATTR             PIC  X(01).             00050004
           05 PO-UPVB0028-EKTONR-ATTR           PIC  X(01).             00050104
           05 PO-UPVB0028-EBVINH-ATTR           PIC  X(01).             00050204
      *                                                                 00051000
      ******** ENDE COPY-MEMBER (DRVB0019) *******************          00060000
