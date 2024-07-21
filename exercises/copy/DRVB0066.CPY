      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB0066.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRVB0066.CPv  $
      *
      *   Rev 2.2   Aug 12 2005 09:44:44   t13ex13
      *AAS00571: VB-Bestandsstatistik
      *
      *   Rev 2.1   Feb 18 2003 15:12:56   t13ex57
      *FVB00458 - Summen in der VB Bestands-Statistik unplausibel  
      *
      *   Rev 2.0   Feb 27 1999 15:57:56   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.0   Nov 04 1995 09:16:20   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRVB0066                                      *
      *    ----------------------------                               *
      *    SATZBESCHREIBUNG ARCHIVDATEI VB-BESTANDSSTATISTIK          *
      *    ----------------------------                               *
      *    ERSTELLT: BERGHOFER                                        *
      *    DATUM:    15.12.1994                                       *
      *****************************************************************
      *    VERSION   1    23.01.95 09:01:59
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AFVB0168!15.12.94! 01 ! NEU ERSTELLT              ! BERGHOFER*
      * AAS00571!01.08.05! 2.2! Neue Spalte ausl. VB      !Gutfleisch*
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      * JAHR      JAHR   "       "            "             "        *
      * MONAT     MONAT DER GESPEICHERTEN SUMMENZEILE GESAMTSTATISTIK*
      * ALLE ANDEREN FELDER BEINHALTEN DIE GEZAEHLTEN WERTE DER VB-  *
      * BESTANDSSTATISTIK  FUER DEN MONAT                            *
      ****************************************************************
       01  DRVB0066-SATZ.
           05  DRVB0066-KEY.
               10  DRVB0066-JAHR             PIC  X(004).
               10  DRVB0066-MONAT            PIC  X(002).
           05  DRVB0066-ANZ-D                PIC  9(005).
           05  DRVB0066-ANZ-RD2              PIC  9(005).
           05  DRVB0066-ANZ-RD1              PIC  9(005).
           05  DRVB0066-ANZ-HGS              PIC  9(005).
           05  DRVB0066-ANZ-GS               PIC  9(005).
           05  DRVB0066-ANZ-D-GS             PIC  9(005).
           05  DRVB0066-ANZ-RGS              PIC  9(005).
           05  DRVB0066-ANZ-AL               PIC  9(005).
           05  DRVB0066-ANZ-VBA              PIC  9(005).
           05  DRVB0066-ANZ-RGS-VBA          PIC  9(005).
           05  DRVB0066-ANZ-D-VBA            PIC  9(005).
010805     05  DRVB0066-ANZ-D-VBA-AUSL       PIC  9(005).
130203*    05  DRVB0066-ANZ-VM-INAKTIV       PIC  9(005).
130203*    05  DRVB0066-ANZ-VM-AKTIV         PIC  9(005).
130203*    05  DRVB0066-ANZ-D-VM             PIC  9(005).
130203     05  DRVB0066-ANZ-VM-INAKTIV       PIC  9(006).
130203     05  DRVB0066-ANZ-VM-AKTIV         PIC  9(006).
130203     05  DRVB0066-ANZ-D-VM             PIC  9(006).
      *
      ******** ENDE COPY-MEMBER (DRVB0066) *******************

