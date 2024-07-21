      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  04.08.2006
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2006 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DBVB0018.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DBVB0018.CPv  $
      *
      *   Rev 2.2   19 Dec 2006 12:26:58   t13ae13
      *AVB00868: VB45 - Host-Pflegedialog DG und V.-Ber.
      *
      *   Rev 2.1   Nov 09 2006 09:11:20   t13ex43
      *AVB00868: VB45 - Host-Pflegedialog DG und V.-Ber.
      *
      *   Rev 2.0   Aug 04 2006 13:32:22   t13ex43
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *   MICRO FOCUS BMS SCREEN PAINTER (VER MFER25)
      *   MAPSET NAME   BVB0018
      *   DATE CREATED  08/31/2006
      *   TIME CREATED  16:39:21

      *  INPUT DATA FOR MAP BVB0018
         01 BVB0018I.
            03 FILLER                         PIC X(12).
            03 EDGL                           PIC S9(4) COMP.
            03 EDGF                           PIC X.
            03 FILLER                         PIC X(1).
            03 EDGI                           PIC X(10).
            03 AVBERL                         PIC S9(4) COMP.
            03 AVBERF                         PIC X.
            03 FILLER                         PIC X(1).
            03 AVBERI                         PIC X(10).
            03 ESEIAKTL                       PIC S9(4) COMP.
            03 ESEIAKTF                       PIC X.
            03 FILLER                         PIC X(1).
            03 ESEIAKTI                       PIC X(3).
            03 ASEIGESL                       PIC S9(4) COMP.
            03 ASEIGESF                       PIC X.
            03 FILLER                         PIC X(1).
            03 ASEIGESI                       PIC X(3).
            03 DFHG001I                       OCCURS 12 TIMES.
               05 EFCL                           PIC S9(4) COMP.
               05 EFCF                           PIC X.
               05 FILLER                         PIC X(1).
               05 EFCI                           PIC X(1).
               05 EDGZL                          PIC S9(4) COMP.
               05 EDGZF                          PIC X.
               05 FILLER                         PIC X(1).
               05 EDGZI                          PIC X(10).
               05 ADIRL                          PIC S9(4) COMP.
               05 ADIRF                          PIC X.
               05 FILLER                         PIC X(1).
               05 ADIRI                          PIC X(3).
               05 AVBNRL                         PIC S9(4) COMP.
               05 AVBNRF                         PIC X.
               05 FILLER                         PIC X(1).
               05 AVBNRI                         PIC X(7).
               05 ADIRNAML                       PIC S9(4) COMP.
               05 ADIRNAMF                       PIC X.
               05 FILLER                         PIC X(1).
               05 ADIRNAMI                       PIC X(30).
               05 ESPRECHL                       PIC S9(4) COMP.
               05 ESPRECHF                       PIC X.
               05 FILLER                         PIC X(1).
               05 ESPRECHI                       PIC X(1).
            03 AFCANZL                        PIC S9(4) COMP.
            03 AFCANZF                        PIC X.
            03 FILLER                         PIC X(1).
            03 AFCANZI                        PIC X(76).

      *  OUTPUT DATA FOR MAP BVB0018
         01 BVB0018O REDEFINES BVB0018I.
            03 FILLER                         PIC X(12).
            03 FILLER                         PIC X(2).
            03 EDGA                           PIC X.
            03 EDGH                           PIC X.
            03 EDGO                           PIC X(10).
            03 FILLER                         PIC X(2).
            03 AVBERA                         PIC X.
            03 AVBERH                         PIC X.
            03 AVBERO                         PIC X(10).
            03 FILLER                         PIC X(2).
            03 ESEIAKTA                       PIC X.
            03 ESEIAKTH                       PIC X.
            03 ESEIAKTO                       PIC X(3).
            03 FILLER                         PIC X(2).
            03 ASEIGESA                       PIC X.
            03 ASEIGESH                       PIC X.
            03 ASEIGESO                       PIC X(3).
            03 DFHG001O OCCURS 12.
               05 FILLER                         PIC X(2).
               05 EFCA                           PIC X.
               05 EFCH                           PIC X.
               05 EFCO                           PIC X(1).
               05 FILLER                         PIC X(2).
               05 EDGZA                          PIC X.
               05 EDGZH                          PIC X.
               05 EDGZO                          PIC X(10).
               05 FILLER                         PIC X(2).
               05 ADIRA                          PIC X.
               05 ADIRH                          PIC X.
               05 ADIRO                          PIC X(3).
               05 FILLER                         PIC X(2).
               05 AVBNRA                         PIC X.
               05 AVBNRH                         PIC X.
               05 AVBNRO                         PIC X(7).
               05 FILLER                         PIC X(2).
               05 ADIRNAMA                       PIC X.
               05 ADIRNAMH                       PIC X.
               05 ADIRNAMO                       PIC X(30).
               05 FILLER                         PIC X(2).
               05 ESPRECHA                       PIC X.
               05 ESPRECHH                       PIC X.
               05 ESPRECHO                       PIC X(1).
            03 FILLER                         PIC X(2).
            03 AFCANZA                        PIC X.
            03 AFCANZH                        PIC X.
            03 AFCANZO                        PIC X(76).

