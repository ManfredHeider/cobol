      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  21.09.2005
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2005 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI1828.CPY  $
      *    $Revision:   2.3  $
      *
      *    $Log:   L:/mf/CPY/DRKI1828.CPv  $
      *
      *   Rev 2.3   May 31 2006 16:51:40   t13ex48
      *FKI00972: Korrektur von Betreuer und Betreuungs-Status in de
      *r EDI-DB
      *
      *   Rev 2.1   Oct 12 2005 14:59:48   t13ex48
      *FKI00972: Korrektur von Betreuer und Betreuungs-Status in de
      *r EDI-DB
      *
      *   Rev 2.0   Sep 21 2005 11:14:00   t13ex48
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *
      *
      *---------------------------------------------------------------*
      *              #####################                             *
      *     ########    D R K I 1 8 2 8    ########                    *
      *              #####################                             *
      *                                                                *
      *  EDI-Korrektur-Datei aus Entladebestand (Oracle-DB)            *
      *                                                                *
      *----------------------------------------------------------------*
      *    LOGBUCH                                                     *
      *DATUM   !      ÄNDERUNG                         !NAME     !VERS.*
      *AUFTRAG !                                       !         !     *
      *--------!---------------------------------------!---------!-----*
      *21.09.05!NEUERSTELLUNG                          !Sebbach  ! 01  *
      *FKI00972!                                       !         !     *
      *--------!---------------------------------------!---------!-----*
170506*17.05.06!Anpassung an AMV-Vertrags-Nr.(12 St.)  !Sebbach  !     *
      *        !                                       !         !     *
      ******************************************************************
       01  DRKI1828.

           05  EDI-TEIL1.
            10  FLLER                       PIC X(001).
            10  EDI-Archiv-Adressse         PIC X(026).
      *            Archiv-Adresse
            10  FLLER                       PIC X(002).
            10  EDI-Ges                     PIC 9(002).
      *            Produktgeber-Gesellschaft (10 = AMV, 20 = AML, usw.)
            10  FLLER                       PIC X(001).
170506*     10  EDI-VNR                     PIC 9(010).
170506      10  EDI-VNR                     PIC 9(012).
      *            Vertrags-Nummer
            10  FLLER                       PIC X(001).
            10  EDI-VBNR                    PIC 9(007).
      *            VB-Nummer des Vertragsbetreuers
            10  FLLER                       PIC X(002).
            10  EDI-Betr-Status             PIC X(001).
      *            Betreuungs-Status ("B", "K", "U")
            10  FLLER                       PIC X(001).
           05  EDI-TEIL2.
            10  FLLER                       PIC X(002).
            10  EDI-Irrl-KZ                 PIC X(001).
      *            Irrlaeufer-KZ  ("N", "Y")
            10  FLLER                       PIC X(002).
            10  EDI-DL-VBNR-X               PIC X(007).
      *    VB-Nummer des DL (nur bei unbetreuten Vertr. gefuellt)
           05  EDI-TEIL3.
            10  FLLER                       PIC X(001).
            10  EDI-VTG-ID                  PIC X(013).
      *     10 FILLER                       PIC X(000).
      *            RESERVE
      *
      *----------- E N D E   D R K I 1 7 3 8 --------------*
