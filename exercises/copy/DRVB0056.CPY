      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB0056.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRVB0056.CPv  $
      *
      *   Rev 2.1   Jun 09 2006 14:33:22   t13ae35
      *ACV00079: Layout Adresse auf DIN anpassen
      *
      *   Rev 2.0   Feb 27 1999 15:57:54   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   17 Oct 1997 11:51:18   T13AE35
      *AFVB0364 - Aenderung, SST PMVB0107-PMVB0203
      *
      *   Rev 1.0   Nov 04 1995 09:14:16   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: SATZBESCHREIBUNG ADRESSDATEN ZU VB            *
      *                 (ZWISCHENDATEI FÜR ETIKETTEN/ADRESSBLÄTTER)   *
      *    --------------------------------------------------------   *
      *    ERSTELLT: SCHÖLLHAMMER                                     *
      *****************************************************************
      *    VERSION   1    09.06.94 10:44:32
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AFVB0114!30.05.94! 01 ! NEU                       ! SCHÖLLH. *
      * -------------------------------------------------------------*
      * AFVB0364!23.07.97! 02 ! EINFÜGEN ZWEIER FELDER    ! HERING   *
      *         !        !    ! AUSKOMMENTIEREN 4 FELDER  !          *
      * -------------------------------------------------------------*
090506* ACV00079!09.05.06! 2.1! Ort+Straße jetzt 40Byte   ! J.Hering *
      *         !        !    ! zusätzlich Landbezeichnung!
      * -------------------------------------------------------------*
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
      *  VB-DATEN, DIE FÜR DIE ERSTELLUNG VON ETIKETTEN ODER VON
      *  ADRESSBLÄTTERN BENÖTIGT WERDEN.
      *
230797*  INPUT FÜR PMVB0203 (STANDARDETIKETTENDRUCK)
230797*  OUTPUT VON PMVB0107 (DATENERMITTLUNG)
090506*  Satzlänge jetzt 231 Byte
       01  DRVB0056-VBADRES-DATEN.
           05  DRVB0056-MMANR           PIC  9(007).
           05  DRVB0056-MPZ             PIC  X(001).
           05  DRVB0056-MDIR-1          PIC  9(003).
           05  DRVB0056-MSTUFE-1        PIC  9(003).
           05  DRVB0056-MZUNAM          PIC  X(020).
           05  DRVB0056-MVNAME          PIC  X(020).
090506     05  DRVB0056-MSTRASSE        PIC  X(040).
           05  DRVB0056-MPLZ            PIC  X(006).
090506     05  DRVB0056-MORT            PIC  X(040).
           05  DRVB0056-MAUSL           PIC  X(003).
230797     05  DRVB0056-ANREDE-ZEILE1   PIC  X(024).
230797     05  DRVB0056-ANREDE-ZEILE2   PIC  X(024).
090506     05  DRVB0056-LANDBEZ         PIC  X(040).
      ******** ENDE COPY-MEMBER (DRVB0056) ***************************

