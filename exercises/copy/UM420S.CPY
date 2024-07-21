      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   UM420S.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\UM420S.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 15:01:00   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.0   Nov 04 1995 09:35:26   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *****  VERSION
      *    *************************************************************
      *    COPY-MEMBER: UM420S (KONTROLLSATZ)
      *    **********************************
      *                                      ERSTELLT: H.WINZEN
      *                                         DATUM: 24.09.90
      *    *************************************************************
CCC009*    SKIP1
       01  KONTROLLSATZ.
           05 KKEY.
              10 KJOB.
                 15 KJOBNAME            PIC X(06).
              10 KVERSION               PIC 9(02).
           05 KREST.
              10 KKZ-START                 PIC 9(01).
              10 KKZ-ENDE                  PIC 9(01).
              10 KKZ-MANUELL               PIC 9(01).
              10 KTAB.
                 15 KSTEP                  PIC 9(01) OCCURS 20.
                 15 KDATUM.
                    20 KJJ                 PIC 9(02).
                    20 KMM                 PIC 9(02).
                    20 KTT                 PIC 9(02).
                 15 KBEGINN.
                    20 KHHB                PIC 9(02).
                    20 KMMB                PIC 9(02).
                    20 KSSB                PIC 9(02).
                 15 KENDE.
                    20 KHHE                PIC 9(02).
                    20 KMME                PIC 9(02).
                    20 KSSE                PIC 9(02).
           05 KSTAT-DAT.
              10 KSTAT-JJ                  PIC 9(02).
              10 KSTAT-MM                  PIC 9(02).
              10 KSTAT-TT                  PIC 9(02).
           05 FILLER                       PIC X(25).
