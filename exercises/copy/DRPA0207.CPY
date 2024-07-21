      *--------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *     Datum   :  November 1995
      *     Kopf aus:  N:PROFILEKOPFCBL.DAT
      *--------------------------------------------------------------*
      *
      *--------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *--------------------------------------------------------------*
      *    $Workfile:   DRPA0207.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRPA0207.CPv  $
      *
      *   Rev 2.1   Sep 29 2017 14:20:40   T13AE34
      *ARZ00049: Reaktivierung drpa0207.cpy
      *
      *   Rev 2.0   Feb 27 1999 14:28:16   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.2   06 Aug 1998 12:38:56   T01JM2Y
      *APAA0032 - PEW-Release 2.0
      *
      *   Rev 1.1   Apr 09 1997 16:51:56   T13AE31
      *EINFUEHRUNG PEW
      *
      *   Rev 1.0   Feb 19 1997 12:09:38   P13PVCS
      *Initial revision.
      *
      *--------------------------------------------------------------*
      ****************************************************************
      *   COPY: DRPA0207 ERWEITERUNGEN FÜR WORKFLOW PE-STRUKTUR      *
      *   -----------------------------------------------------------*
      *   ERSTELLT: SCHÄFER                                          *
      *   DATUM:    29.01.1997                                       *
      ****************************************************************
      *    VERSION   01                                              *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AFPA0002!29.01.97! 01 ! NEU                       ! SCHäFER  *
      *         !        !    !                           !          *
      * PEW REL.!25.05.98! 02 ! GUELTIG-AM DATUM ENTFÄLLT ! SCHRIEBER*
      *     2.0 !        !    ! STRUKTURERFASSUNGSDATUM   !          *
      *         !        !    ! ALS EINGABEFELD           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY WIRD IM DIALOG PA11 (PGPA0005) VERWENDET.     *
      *    ES ENTHäLT ERWEITERUNGEN FüR DIE FELDER DES WORKFLOW      *
      *    PE-STRUKTUR.                                              *
      *                                                              *
      *    - EINIGE BEZEICHNUNGEN UND TEXTE                          *
      *      -  BEZEICHNUNG DES EMPFäNGER-TYP                        *
      *      -  NAME JE PROVISIONSEMPFäNGER                          *
      *                                                              *
      *    - FüR EINGABEFELDER (INTENSIV, OFFEN UND AENDERUNG-KZ)    *
      *      -  ÄNDERUNGS-KZ: ÄNDERUNGS-'*', UM SIE AUF DER MAP      *
      *         DARZUSTELLEN UND ALS ÜBERGABEBEREICH FüR PA12        *
      *                                                              *
      *    - FüR AUSGABEFELDER (INTENSIV)                            *
      *                                                              *
      ****************************************************************
       01 PW-DRPA0207.
250598*
250598*   05  GUELTIG-AM-DAT.
250598*       10 GUELTIG-AM-TT                   PIC X(02).
250598*       10 PUNKT                           PIC X(01).
250598*       10 GUELTIG-AM-MM                   PIC X(02).
250598*       10 PUNKT                           PIC X(01).
250598*       10 GUELTIG-AM-JJJJ                 PIC X(04).
250598*
250598*   05 GUELTIG-AM-DAT-ATTRIBUTE.
250598*      10 FELD-INTENSIV                    PIC  X(001).
250598*      10 FELD-OFFEN                       PIC  X(001).
250598*      10 FELD-AENDERUNG-KZ                PIC  X(001).

250598    05  STRUK-ERF-DAT                      PIC X(10).
250598*
250598    05 STRUK-ERF-DAT-ATTRIBUTE.
250598       10 FELD-INTENSIV                    PIC  X(001).
250598       10 FELD-OFFEN                       PIC  X(001).
250598       10 FELD-AENDERUNG-KZ                PIC  X(001).

          05 PW-DRPA0207-STRUKTUR-TABELLE.
             10 PW-DRPA0207-STRUKTUR-ZEILE   OCCURS 20.

      *         *  PROVISIONSEMPFäNGER
                15 STELLEN-NAME                  PIC  X(020).
                15 MA-ST-NR.
                   20 FELD-INTENSIV              PIC  X(001).
                   20 FELD-OFFEN                 PIC  X(001).
                   20 FELD-AENDERUNG-KZ          PIC  X(001).

      *         *  PRüFZIFFER DES PROV.EMPFäNGER
                15 PRZ.
                   20 FELD-INTENSIV              PIC  X(001).
                   20 FELD-OFFEN                 PIC  X(001).
                   20 FELD-AENDERUNG-KZ          PIC  X(001).

      *         *  STRUKTURSTUFE
                15 EMP-STUFE.
                   20 FELD-INTENSIV              PIC  X(001).
                   20 FELD-OFFEN                 PIC  X(001).
                   20 FELD-AENDERUNG-KZ          PIC  X(001).

      *         *  DIFFERENZ-PROVISIONS-STUFE
                15 DIFF-PROV-STUFE.
                   20 FELD-INTENSIV              PIC  X(001).
                   20 FELD-OFFEN                 PIC  X(001).
                   20 FELD-AENDERUNG-KZ          PIC  X(001).

      *         *  EMPFäNGER-TYP
                15 KNR-EMP-TYP-BEZ               PIC  X(010).
                15 KNR-EMP-TYP.
                   20 FELD-INTENSIV              PIC  X(001).
                   20 FELD-OFFEN                 PIC  X(001).
                   20 FELD-AENDERUNG-KZ          PIC  X(001).
      *
      ******** ENDE COPY-MEMBER DRPA0207 *****************************

