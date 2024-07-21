      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Januar 2001
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 2001 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI1301.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/cpy/DRKI1301.CPv  $
      *
      *   Rev 2.1   16 Oct 2001 14:12:38   t13ex49
      *AKI00242 DTA-Satzstruktur
      *
      *   Rev 2.0   18 Sep 2001 10:03:58   p13sp16
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI1301                                      *
      *    --------------------------------------------------------   *
      *    DMB - Satzaufbau DTA
      *    --------------------------------------------------------   *
      ****************************************************************
      *   erzeugt mit genOZP - (c) PMSmicado  28.10.99/19.23.01
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AKI00242!28.09.01!    ! erstellt aus UU078S       !Peter Pohl*
      *         !        !    ! Prämie geändert           !          *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
      *    Dieses Copy enthält den Satzaufbau der
      *    DMB-Aktualisierungsdatei
      *
      *    Länge: 450 Byte
      *
      ****************************************************************
       01  DRKI1301.

         05  DMB-CSATZ.
           10  DMB-SA                  PIC X.
           10  DMB-VNR                 PIC 9(12).
           10  DMB-ORGANR2             PIC 9(8).
           10  DMB-BAKKZ               PIC X.
           10  DMB-VBEG1.
               15  DMB-VBEG-JJ         PIC 99.
               15  DMB-VBEG            PIC 9(6).
           10  DMB-VABL1.
               15  DMB-VABL-JJ         PIC 99.
               15  DMB-VABL            PIC 9(6).
           10  DMB-AEND1.
               15  DMB-AEND-JJ         PIC XX.
               15  DMB-AEND            PIC X(6).
           10  DMB-ZAHLW               PIC 9.
           10  DMB-FAEL                PIC 99.
           10  DMB-LAST                PIC X.
           10  DMB-VSU.
               15  DMB-VSU-T           PIC 9(6).
               15  DMB-VSU-H           PIC 9(3).
           10  DMB-ANREDE              PIC X.
           10  DMB-TITEL               PIC X.
           10  DMB-NAME                PIC X(30).
           10  DMB-AUSL                PIC X(3).
           10  DMB-PLZ.
               20  DMB-PLZ1            PIC X.
               20  DMB-PLZ2            PIC X(4).
           10  DMB-ORT                 PIC X(25).
           10  DMB-STR                 PIC X(30).
           10  DMB-GEBD1.
               15  DMB-GEBD-JJ         PIC XX.
               15  DMB-GEBD            PIC X(6).
      *    10  DMB-PRAEMIE             PIC 9(8)V9.
           10  DMB-PRAEMIE             PIC 9(8)V99.
      *    10  FILLER                  PIC X(26).
           10  FILLER                  PIC X(25).
           10  DMB-RISZAEHL            PIC 9(3).
           10  DMB-VARIABEL OCCURS 05.
               20 DMB-WAG              PIC X(4).
               20 DMB-VTEXT            PIC X(25).
               20 DMB-VART             PIC 9(5).
               20 FILLER               PIC X(16).

         05  DMB-ASATZ  REDEFINES  DMB-CSATZ.
           10  A-FREI-01               PIC X.
           10  DMB-ABS                 PIC X(33).
           10  DMB-ERSTDAT.
               15  DMB-ERST-JJ         PIC 9(2).
               15  DMB-ERST            PIC 9(6).
           10  DMB-EMPF                PIC X(31).
           10  DMB-INHALT              PIC X(31).
           10  A-FREI-02               PIC X(346).

         05  DMB-ESATZ  REDEFINES  DMB-CSATZ.
           10  C-FREI-01               PIC X.
           10  DMB-SATZANZ             PIC 9(7).
           10  C-FREI-02               PIC X(442).

