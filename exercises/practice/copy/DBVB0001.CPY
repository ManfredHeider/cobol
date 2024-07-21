      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Januar 1999
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1999 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DBVB0001.CPY  $
      *    $Revision:   2.4  $
      *
      *    $Log:   L:/mf/CPY/DBVB0001.CPv  $
      *
      *   Rev 2.4   Feb 13 2008 09:12:24   t13ae34
      *AVB00959: Anzeige von Allfinanz-VB in VB 51
      *
      *   Rev 2.3   May 06 2002 10:10:38   t13ae23
      *AVB00600 - Anzeigen Ortsnamen -
      *
      *   Rev 2.2   Jan 04 2002 09:28:24   t13ae23
      *AVB00519 - VB51 2. Edition -
      *
      *   Rev 2.1   16 Jun 2000 11:26:10   T13AE34
      *AVB00483-Dialog VB51 
      *
      *   Rev 2.0   May 08 2000 08:20:44   P13SP20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DBVB0001                                     *
      *    --------------------------------------------------------  *
      *    ERSTELLT: RAHNER                                          *
      *    DATUM:    16.05.2000                                      *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AVB00483!16.05.00!2.1 ! NEU                       ! RAHNER   *
      * AVB00519!04.01.02!2.2 ! VB51 2.RELESE             ! RAHNER   *
      * AVB00600!06.05.02!2.3 ! ANZEIGE ORTSNAME          ! RAHNER   *
      * AVB00959!08.02.08!2.4 ! VERTRIEBSGESELLSCHAFT     ! HEIDER   *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *  INPUT DATA FOR MAP BVB0036
      *   MICRO FOCUS MAINFRAME EXPRESS  3.0.75
      *   MICRO FOCUS BMS SCREEN PAINTER (VER COBC_1-1-37-31)
      *   MAPSET NAME   BVB0036
      *   DATE CREATED  02/08/2008
      *   TIME CREATED  11:28:37

      *  INPUT DATA FOR MAP BVB0036
         01 BVB0036I.

            03 FILLER                         PIC X(12).
            03 EPLZL                          PIC S9(4) COMP.
            03 EPLZF                          PIC X.
            03 FILLER REDEFINES EPLZF.
               05 EPLZA                          PIC X.
            03 FILLER                         PIC X(1).
            03 EPLZI                          PIC X(5).
            03 ESTVONL                        PIC S9(4) COMP.
            03 ESTVONF                        PIC X.
            03 FILLER REDEFINES ESTVONF.
               05 ESTVONA                        PIC X.
            03 FILLER                         PIC X(1).
            03 ESTVONI                        PIC X(4).
            03 ESTBISL                        PIC S9(4) COMP.
            03 ESTBISF                        PIC X.
            03 FILLER REDEFINES ESTBISF.
               05 ESTBISA                        PIC X.
            03 FILLER                         PIC X(1).
            03 ESTBISI                        PIC X(4).
            03 ASEIFBL                        PIC S9(4) COMP.
            03 ASEIFBF                        PIC X.
            03 FILLER REDEFINES ASEIFBF.
               05 ASEIFBA                        PIC X.
            03 FILLER                         PIC X(1).
            03 ASEIFBI                        PIC X(6).
            03 ESEIAKTL                       PIC S9(4) COMP.
            03 ESEIAKTF                       PIC X.
            03 FILLER REDEFINES ESEIAKTF.
               05 ESEIAKTA                       PIC X.
            03 FILLER                         PIC X(1).
            03 ESEIAKTI                       PIC X(3).
            03 ASEISTRL                       PIC S9(4) COMP.
            03 ASEISTRF                       PIC X.
            03 FILLER REDEFINES ASEISTRF.
               05 ASEISTRA                       PIC X.
            03 FILLER                         PIC X(1).
            03 ASEISTRI                       PIC X(1).
            03 ASEIGESL                       PIC S9(4) COMP.
            03 ASEIGESF                       PIC X.
            03 FILLER REDEFINES ASEIGESF.
               05 ASEIGESA                       PIC X.
            03 FILLER                         PIC X(1).
            03 ASEIGESI                       PIC X(3).
            03 EEINHL                         PIC S9(4) COMP.
            03 EEINHF                         PIC X.
            03 FILLER REDEFINES EEINHF.
               05 EEINHA                         PIC X.
            03 FILLER                         PIC X(1).
            03 EEINHI                         PIC X(7).
            03 EDIRL                          PIC S9(4) COMP.
            03 EDIRF                          PIC X.
            03 FILLER REDEFINES EDIRF.
               05 EDIRA                          PIC X.
            03 FILLER                         PIC X(1).
            03 EDIRI                          PIC X(3).

            03 ZEILEI OCCURS 10.

               05 EFCL                           PIC S9(4) COMP.
               05 EFCF                           PIC X.
               05 FILLER REDEFINES EFCF.
                  10 EFCA                        PIC X.
               05 FILLER                         PIC X(1).
               05 EFCI                           PIC X(1).

               05 APLZL                          PIC S9(4) COMP.
               05 APLZF                          PIC X.
               05 FILLER REDEFINES APLZF.
                  10 APLZA                       PIC X.
               05 FILLER                         PIC X(1).
               05 APLZI                          PIC X(5).

               05 AORTL                          PIC S9(4) COMP.
               05 AORTF                          PIC X.
               05 FILLER REDEFINES AORTF.
                  10 AORTA                       PIC X.
               05 FILLER                         PIC X(1).
               05 AORTI                          PIC X(12).

               05 ADIRL                          PIC S9(4) COMP.
               05 ADIRF                          PIC X.
               05 FILLER REDEFINES ADIRF.
                  10 ADIRA                       PIC X.
               05 FILLER                         PIC X(1).
               05 ADIRI                          PIC X(3).

               05 AVBNRL                         PIC S9(4) COMP.
               05 AVBNRF                         PIC X.
               05 FILLER REDEFINES AVBNRF.
                  10 AVBNRA                      PIC X.
               05 FILLER                         PIC X(1).
               05 AVBNRI                         PIC X(7).

               05 AZUNAMEL                       PIC S9(4) COMP.
               05 AZUNAMEF                       PIC X.
               05 FILLER REDEFINES AZUNAMEF.
                  10 AZUNAMEA                    PIC X.
               05 FILLER                         PIC X(1).
               05 AZUNAMEI                       PIC X(15).

               05 ASTUFEL                        PIC S9(4) COMP.
               05 ASTUFEF                        PIC X.
               05 FILLER REDEFINES ASTUFEF.
                  10 ASTUFEA                     PIC X.
               05 FILLER                         PIC X(1).
               05 ASTUFEI                        PIC X(10).

               05 AVGESL                         PIC S9(4) COMP.
               05 AVGESF                         PIC X.
               05 FILLER REDEFINES AVGESF.
                  10 AVGESA                      PIC X.
               05 FILLER                         PIC X(1).
               05 AVGESI                         PIC X(6).

            03 AFCANZL                        PIC S9(4) COMP.
            03 AFCANZF                        PIC X.
            03 FILLER REDEFINES AFCANZF.
               05 AFCANZA                        PIC X.
            03 FILLER                         PIC X(1).
            03 AFCANZI                        PIC X(79).

      *  OUTPUT DATA FOR MAP BVB0036
         01 BVB0036O REDEFINES BVB0036I.

            03 FILLER                         PIC X(12).

            03 FILLER                         PIC X(3).
            03 EPLZH                          PIC X.
            03 EPLZO                          PIC X(5).

            03 FILLER                         PIC X(3).
            03 ESTVONH                        PIC X.
            03 ESTVONO                        PIC X(4).

            03 FILLER                         PIC X(3).
            03 ESTBISH                        PIC X.
            03 ESTBISO                        PIC X(4).

            03 FILLER                         PIC X(3).
            03 ASEIFBH                        PIC X.
            03 ASEIFBO                        PIC X(6).

            03 FILLER                         PIC X(3).
            03 ESEIAKTH                       PIC X.
            03 ESEIAKTO                       PIC X(3).

            03 FILLER                         PIC X(3).
            03 ASEISTRH                       PIC X.
            03 ASEISTRO                       PIC X(1).

            03 FILLER                         PIC X(3).
            03 ASEIGESH                       PIC X.
            03 ASEIGESO                       PIC X(3).

            03 FILLER                         PIC X(3).
            03 EEINHH                         PIC X.
            03 EEINHO                         PIC X(7).

            03 FILLER                         PIC X(3).
            03 EDIRH                          PIC X.
            03 EDIRO                          PIC X(3).

            03 ZEILEO OCCURS 10.

               05 FILLER                         PIC X(3).
               05 EFCH                           PIC X.
               05 EFCO                           PIC X(1).

               05 FILLER                         PIC X(3).
               05 APLZH                          PIC X.
               05 APLZO                          PIC X(5).

               05 FILLER                         PIC X(3).
               05 AORTH                          PIC X.
               05 AORTO                          PIC X(12).

               05 FILLER                         PIC X(3).
               05 ADIRH                          PIC X.
               05 ADIRO                          PIC X(3).

               05 FILLER                         PIC X(3).
               05 AVBNRH                         PIC X.
               05 AVBNRO                         PIC X(7).

               05 FILLER                         PIC X(3).
               05 AZUNAMEH                       PIC X.
               05 AZUNAMEO                       PIC X(15).

               05 FILLER                         PIC X(3).
               05 ASTUFEH                        PIC X.
               05 ASTUFEO                        PIC X(10).

               05 FILLER                         PIC X(3).
               05 AVGESH                         PIC X.
               05 AVGESO                         PIC X(6).

            03 FILLER                         PIC X(3).
            03 AFCANZH                        PIC X.
            03 AFCANZO                        PIC X(79).

