      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI0832.CPY  $
      *    $Revision:   2.4  $
      *
      *    $Log:   L:/mf/CPY/DRKI0832.CPv  $
      *
      *   Rev 2.4   Dec 26 2007 13:16:56   t13ex48
      *AKL00666: Anpassung RUECK-DTA für Allfinanz: PMKI0294
      *
      *   Rev 2.2   31 Jul 2000 15:50:46   T13EX48
      *AKI00118
      *Erweiterung um Geburtsdatum
      *
      *   Rev 2.1   Jun 14 1999 15:14:24   T13EX48
      *AKI31
      *RDTA VLK fuer File-Tran-Aufbereitung
      *
      *
      *   Rev 2.0   Feb 27 1999 12:58:48   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.0   12 Jan 1999 15:50:28   P13SP20
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *****  VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AKI00006!30.04.99! 01 ! NEUERSTELLUNG             !HSE       *
      * -------------------------------------------------------------*
      * AKI00118!26.03.00! 02 ! Zusaetzl. Laenge fuer     !HSE       *
      *         !        !    ! RDTA1-Ausgabe             !          *
      *---------!--------!----!---------------------------!----------*
191207* AKL00666!19.12.07! 03 ! Neu: Vertriebs-Gesellsch. ! Sebbach  *
      *         !        !    ! Datum auskommentiert      !          *
      *---------!--------!----!---------------------------!----------*
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *****
      *****        COPY-MEMBER  DRKI0832
      *****        - VORLAUFKARTE FUER PROGRAMM PMKI0294           *****
      *****        - LAENGE     80 BYTES                           *****
      ******************************************************************
       01  DRKI0832-VLK.
               10  DRKI0832-KENNUNG          PIC X(11).
               10  FILLER                    PIC X(01).
               10  DRKI0832-GES              PIC X(03).
               10  FILLER                    PIC X(01).
               10  DRKI0832-DTA-ART          PIC X(05).
230600         10  FILLER                    PIC X(01).
230600         10  DRKI0832-LAENGE           PIC 9(05).
               10  FILLER                    PIC X(01).
191207*        10  DRKI0832-DATUM            PIC X(10).
191207         10  DRKI0832-VTGes            PIC X(10).
191207*            Vertriebsgesellschaft
191207*            Zulaessig "DVAG      " oder "ALLFINANZ "
230600*        10  FILLER                    PIC X(48).
230600         10  FILLER                    PIC X(42).

      *****   ENDE     COPY-MEMBER  DRKI0832                      ******


