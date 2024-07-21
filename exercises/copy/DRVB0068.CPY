      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB0068.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRVB0068.CPv  $
      *
      *   Rev 2.1   Aug 12 2005 09:44:36   t13ex13
      *AAS00571: VB-Bestandsstatistik
      *
      *   Rev 2.0   Feb 27 1999 15:57:56   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.0   Nov 04 1995 09:16:46   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRVB0068                                      *
      *    --------------------------------------------------------   *
      *    SATZBESCHREIBUNG ZWISCHENDATEI VB-BESTANDSSTATISTIK        *
      *    --------------------------------------------------------   *
      *    ERSTELLT: BERGHOFER                                        *
      *    DATUM:    15.12.1994                                       *
      *****************************************************************
      *    VERSION   3    28.04.95 17:29:47
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AFVB0168!15.12.94! 01 ! NEU ERSTELLT              ! BERGHOFER*
      * AFVB0194!27.04.95! 02 ! GESELLSCHAFT DVAG ('D')   !          *
      *         !        !    !              AVBG ('A')   ! GOECKE   *
      * AAS00571!27.07.05! 2.1! Erweitern um ausl. VB     !Gutfleisch*
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      * DL-DIR-NR         NUMMER DER DIREKTION AUS DL                *
      * NAME-VNAME        NAME UND ABGEKUERZTER VORNAME DES D-LEITER *
      * BEZ-REGION        BEZEICHNUNG DER REGION IM KLARTEXT         *
      * ALLE ANDEREN FELDER ENTHALTEN DIE GEZAEHLTEN WERTE DER VB-   *
      * BESTANDSSTATISTIK FUER DIE DIREKTION                         *
      *                                                              *
      * STICHTAG          BEZUGSTAG DER AUSWERTUNG                   *
      * DUMMY-REGION      IST SPACE                                  *
      * STICHTAG-FLAG     "V" WENN STICHTAG UEBER VORLAUFKARTE EIN-  *
      *                   GESTELLT WURDE SONST SPACE                 *
      ****************************************************************
       01  DRVB0068-SATZ.
           05  DRVB0068-DL-DIR-NR                PIC S9(004) COMP.
           05  DRVB0068-SATZ-DATEN.
               10  DRVB0068-MAN-VNAME-NAME       PIC  X(016).
               10  DRVB0068-ANZ-D                PIC  9(005).
               10  DRVB0068-ANZ-RD2              PIC  9(005).
               10  DRVB0068-ANZ-RD1              PIC  9(005).
               10  DRVB0068-ANZ-HGS              PIC  9(005).
               10  DRVB0068-ANZ-GS               PIC  9(005).
               10  DRVB0068-ANZ-D-GS             PIC  9(005).
               10  DRVB0068-ANZ-RGS              PIC  9(005).
               10  DRVB0068-ANZ-AL               PIC  9(005).
               10  DRVB0068-ANZ-VBA              PIC  9(005).
               10  DRVB0068-ANZ-RGS-VBA          PIC  9(005).
               10  DRVB0068-ANZ-D-VBA            PIC  9(005).
270705         10  DRVB0068-ANZ-D-VBA-AUSL       PIC  9(005).
               10  DRVB0068-ANZ-VM-INAKTIV       PIC  9(005).
               10  DRVB0068-ANZ-VM-AKTIV         PIC  9(005).
               10  DRVB0068-ANZ-D-VM             PIC  9(005).
               10  DRVB0068-KNR-BEZ-REGION       PIC  X(020).

           05  DRVB0068-DUMMY-DATEN REDEFINES DRVB0068-SATZ-DATEN.
               10  DRVB0068-STICHTAG             PIC  X(010).
               10  DRVB0068-STICHTAG-FLAG        PIC  X(001).
270495         10  DRVB0068-GESELLSCHAFT         PIC  X(001).
270705*        10  FILLER                        PIC  X(074).
270705         10  FILLER                        PIC  X(079).
               10  DRVB0068-DUMMY-REGION         PIC  X(020).
      *
      ******** ENDE COPY-MEMBER (DRVB0068) *******************

