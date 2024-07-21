      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Januar 1999
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1999 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI1163.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:\MF\CPY\DRKI1163.CPv  $
      *
      *   Rev 2.1   05 Feb 2001 15:41:10   T13AE44
      * 
      *
      *   Rev 2.0   07 Dec 2000 09:36:10   p13sp16
      *Archiverstellung
      *
      *----------------------------------------------------------------*
      *
      *    COPY-MEMBER FUER Auswertung Butter / fuer Transfer in       *
      *                                         csv - Datei            *
      *    ----------------------------------------------------------- *
      *                                                                *
      *    LAENGE = 239 Bytes       ERSTELLT AM 22.01.01.              *
      *                                                                *
      *----------------------------------------------------------------*
      *    LOGBUCH                                                     *
      *DATUM   !      ÄNDERUNG                         !NAME     !VERS.*
      *AUFTRAG !                                       !         !     *
      *--------!---------------------------------------!---------!-----*
      *        !                                       !         !     *
      *        !                                       !         !     *
      *        !                                       !         !     *
      ******************************************************************
       01  DRKI1163.
         05 DRKI1163-DATEN.
      *    Direktion: ZELLE1=DIR-Nr., ZELLE2=Name des Dir.leiters      *
           10  ZELLE1         PIC X(15).
           10  KOMMA1         PIC X.
           10  ZELLE2         PIC X(15).
           10  KOMMA2         PIC X.
      *    Zelle3=Anzahl VB's ab VBA                                   *
           10  ZELLE3         PIC X(15).
           10  KOMMA3         PIC X.
           10  ZELLE4         PIC X(15).
           10  KOMMA4         PIC X.
           10  ZELLE5         PIC X(15).
           10  KOMMA5         PIC X.
           10  ZELLE6         PIC X(15).
           10  KOMMA6         PIC X.
           10  ZELLE7         PIC X(15).
           10  KOMMA7         PIC X.
           10  ZELLE8         PIC X(15).
           10  KOMMA8         PIC X.
           10  ZELLE9         PIC X(15).
           10  KOMMA9         PIC X.
           10  ZELLE10        PIC X(15).
           10  KOMMA10        PIC X.
           10  ZELLE11        PIC X(15).
           10  KOMMA11        PIC X.
           10  ZELLE12        PIC X(15).
           10  KOMMA12        PIC X.
           10  ZELLE13        PIC X(15).
           10  KOMMA13        PIC X.
           10  ZELLE14        PIC X(15).
           10  KOMMA14        PIC X.
           10  ZELLE15        PIC X(15).
         05 DRKI1163-UEBERSCHR REDEFINES DRKI1163-DATEN.
           10  ZELLE1-UEBERSCHR PIC X(15).
           10  KOMMA1-UEBERSCHR PIC X.
           10  ZELLE2-UEBERSCHR PIC X(15).
           10  KOMMA2-UEBERSCHR PIC X.
           10  ZELLE3-UEBERSCHR PIC X(15).
           10  KOMMA3-UEBERSCHR PIC X.
           10  ZELLE4-UEBERSCHR PIC X(191).
         05 DRKI1163-DATUM     REDEFINES DRKI1163-DATEN.
           10  ZELLE1-DATUM-TEXT PIC X(15).
           10  KOMMA1-DATUM-TEXT PIC X.
           10  ZELLE2-DATUM      PIC X(15).
           10  KOMMA2-DATUM      PIC X.
           10  ZELLE3-DATUM-REST PIC X(207).
      *
      *---------------------------------------------------------------*
