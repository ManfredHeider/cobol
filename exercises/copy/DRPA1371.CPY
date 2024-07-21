      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Januar 1999
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1999 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA1371.CPY  $
      *    $Revision:   2.3  $
      *
      *    $Log:   L:/mf/CPY/DRPA1371.CPv  $
      *
      *   Rev 2.3   16 May 2007 06:45:40   t13ex44
      *APA00604: Anpassungen in Daisy zur Ablösung PEW
      *
      *   Rev 2.2   22 Nov 2000 09:47:56   T13EX43
      *APA00112 : Nachtrag DIT (VNR-Konvertierung)
      *
      *   Rev 2.1   Apr 13 2000 14:52:46   T13EX21
      *APA00067 - Neuerstellung
      *
      *   Rev 2.0   Apr 11 2000 16:57:50   P13SP20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRPA1371  FUNKTION EXT. VNR IN DAISY-FORMAT  *
      *                                    FORMATIEREN               *
      *    -------------------------------------------------------   *
      *    ERSTELLT: STORCH                                          *
      *    DATUM:    29.03.2000                                      *
      ****************************************************************
      *    VERSION   02
      ****************************************************************
      * AENDERUNGEN:                                                 *
      *                                                              *
      * AUFTRAG ! DATUM  !VER  ! AENDERUNG                 ! NAME    *
      * -------------------------------------------------------------*
      * APA00067!29.03.00!2.1  ! NEUERSTELLUNG             ! STORCH  *
      * APA00112!20.11.00!2.2  ! RC-UNZ HINZU              !KLEEMEIER*
      * APA00604!12.04.07!2.3  ! FREMDSYS ANTRAG HINZU     ! SCHEIDEL*
      *         !        !     !                           !         *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
       01 PI-DRPA1371.
          05 PI-DRPA1371-PGEB-GES                PIC S9(004) COMP.
          05 PI-DRPA1371-EXT-VNR                 PIC  X(015).
120407    05 PI-DRPA1371-EXT-VNR-AN    REDEFINES PI-DRPA1371-EXT-VNR
120407                                 PIC S9(015)       PACKED-DECIMAL.
          05 PI-DRPA1371-KNR-FREMDSYSTEM         PIC S9(004) COMP.
      *
       01 PO-DRPA1371.
          05 PO-DRPA1371-RC                      PIC  X(003).
             88 PO-DRPA1371-RC-OK                        VALUE 'OK '.
201100       88 PO-DRPA1371-RC-UNZ                       VALUE 'UNZ'.
          05 PO-DRPA1371-DSY-EXT-VNR             PIC  X(015).
      *
      ****** ENDE COPY-MEMBER ****************************************
