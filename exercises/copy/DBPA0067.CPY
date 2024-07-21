      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  17.07.2015
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2015 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DBPA0067.CPY  $
      *    $Revision:   2.3  $
      *
      *    $Log:   L:/mf/CPY/DBPA0067.CPv  $
      *
      *   Rev 2.3   Sep 27 2016 09:29:54   t13ae46
      *APA02280: Dialoganpassungen PG14
      *
      *   Rev 2.0   Jul 17 2015 14:03:08   t13ae46
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *   Mainframe Express 3.1  6.013.0041
      *   Micro Focus BMS Screen Painter (ver IDE_6-0-57)
      *   MapSet Name   BPA0067
      *   Date Created  09/27/2016
      *   Time Created  09:25:26

      *  Input Data For Map BPA0067
         01 BPA0067I.
            03 FILLER                         PIC X(12).
            03 AGESL                          PIC S9(4) COMP.
            03 AGESF                          PIC X.
            03 FILLER                         PIC X(1).
            03 AGESI                          PIC X(2).
            03 AGESBML                        PIC S9(4) COMP.
            03 AGESBMF                        PIC X.
            03 FILLER                         PIC X(1).
            03 AGESBMI                        PIC X(10).
            03 ASEIFBL                        PIC S9(4) COMP.
            03 ASEIFBF                        PIC X.
            03 FILLER                         PIC X(1).
            03 ASEIFBI                        PIC X(6).
            03 ESEIAKTL                       PIC S9(4) COMP.
            03 ESEIAKTF                       PIC X.
            03 FILLER                         PIC X(1).
            03 ESEIAKTI                       PIC X(4).
            03 ASEISTRL                       PIC S9(4) COMP.
            03 ASEISTRF                       PIC X.
            03 FILLER                         PIC X(1).
            03 ASEISTRI                       PIC X(1).
            03 ASEIGESL                       PIC S9(4) COMP.
            03 ASEIGESF                       PIC X.
            03 FILLER                         PIC X(1).
            03 ASEIGESI                       PIC X(4).
            03 AUEBDATL                       PIC S9(4) COMP.
            03 AUEBDATF                       PIC X.
            03 FILLER                         PIC X(1).
            03 AUEBDATI                       PIC X(10).
            03 EFCSL                          PIC S9(4) COMP.
            03 EFCSF                          PIC X.
            03 FILLER                         PIC X(1).
            03 EFCSI                          PIC X(3).
            03 AFCSANZL                       PIC S9(4) COMP.
            03 AFCSANZF                       PIC X.
            03 FILLER                         PIC X(1).
            03 AFCSANZI                       PIC X(17).
            03 EPRODL                         PIC S9(4) COMP.
            03 EPRODF                         PIC X.
            03 FILLER                         PIC X(1).
            03 EPRODI                         PIC X(3).
            03 AFCFBL                         PIC S9(4) COMP.
            03 AFCFBF                         PIC X.
            03 FILLER                         PIC X(1).
            03 AFCFBI                         PIC X(2).
            03 AFABFVL                        PIC S9(4) COMP.
            03 AFABFVF                        PIC X.
            03 FILLER                         PIC X(1).
            03 AFABFVI                        PIC X(10).
            03 AFABFBL                        PIC S9(4) COMP.
            03 AFABFBF                        PIC X.
            03 FILLER                         PIC X(1).
            03 AFABFBI                        PIC X(10).
            03 APWAEBKL                       PIC S9(4) COMP.
            03 APWAEBKF                       PIC X.
            03 FILLER                         PIC X(1).
            03 APWAEBKI                       PIC X(4).
            03 DFHG001I                       OCCURS 11 TIMES.
               05 EFCZL                          PIC S9(4) COMP.
               05 EFCZF                          PIC X.
               05 FILLER                         PIC X(1).
               05 EFCZI                          PIC X(1).
               05 APLUSL                         PIC S9(4) COMP.
               05 APLUSF                         PIC X.
               05 FILLER                         PIC X(1).
               05 APLUSI                         PIC X(1).
               05 AVNRL                          PIC S9(4) COMP.
               05 AVNRF                          PIC X.
               05 FILLER                         PIC X(1).
               05 AVNRI                          PIC X(15).
               05 AVSTATL                        PIC S9(4) COMP.
               05 AVSTATF                        PIC X.
               05 FILLER                         PIC X(1).
               05 AVSTATI                        PIC X(4).
               05 APRODS1L                       PIC S9(4) COMP.
               05 APRODS1F                       PIC X.
               05 FILLER                         PIC X(1).
               05 APRODS1I                       PIC X(3).
               05 ANETBTRL                       PIC S9(4) COMP.
               05 ANETBTRF                       PIC X.
               05 FILLER                         PIC X(1).
               05 ANETBTRI                       PIC X(13).
               05 AUSTBTRL                       PIC S9(4) COMP.
               05 AUSTBTRF                       PIC X.
               05 FILLER                         PIC X(1).
               05 AUSTBTRI                       PIC X(13).
               05 AFAEVDAL                       PIC S9(4) COMP.
               05 AFAEVDAF                       PIC X.
               05 FILLER                         PIC X(1).
               05 AFAEVDAI                       PIC X(10).
               05 AFAEBDAL                       PIC S9(4) COMP.
               05 AFAEBDAF                       PIC X.
               05 FILLER                         PIC X(1).
               05 AFAEBDAI                       PIC X(10).
            03 AFCZANZL                       PIC S9(4) COMP.
            03 AFCZANZF                       PIC X.
            03 FILLER                         PIC X(1).
            03 AFCZANZI                       PIC X(79).

      *  Output Data For Map BPA0067
         01 BPA0067O REDEFINES BPA0067I.
            03 FILLER                         PIC X(12).
            03 FILLER                         PIC X(2).
            03 AGESA                          PIC X.
            03 AGESH                          PIC X.
            03 AGESO                          PIC X(2).
            03 FILLER                         PIC X(2).
            03 AGESBMA                        PIC X.
            03 AGESBMH                        PIC X.
            03 AGESBMO                        PIC X(10).
            03 FILLER                         PIC X(2).
            03 ASEIFBA                        PIC X.
            03 ASEIFBH                        PIC X.
            03 ASEIFBO                        PIC X(6).
            03 FILLER                         PIC X(2).
            03 ESEIAKTA                       PIC X.
            03 ESEIAKTH                       PIC X.
            03 ESEIAKTO                       PIC X(4).
            03 FILLER                         PIC X(2).
            03 ASEISTRA                       PIC X.
            03 ASEISTRH                       PIC X.
            03 ASEISTRO                       PIC X(1).
            03 FILLER                         PIC X(2).
            03 ASEIGESA                       PIC X.
            03 ASEIGESH                       PIC X.
            03 ASEIGESO                       PIC X(4).
            03 FILLER                         PIC X(2).
            03 AUEBDATA                       PIC X.
            03 AUEBDATH                       PIC X.
            03 AUEBDATO                       PIC X(10).
            03 FILLER                         PIC X(2).
            03 EFCSA                          PIC X.
            03 EFCSH                          PIC X.
            03 EFCSO                          PIC X(3).
            03 FILLER                         PIC X(2).
            03 AFCSANZA                       PIC X.
            03 AFCSANZH                       PIC X.
            03 AFCSANZO                       PIC X(17).
            03 FILLER                         PIC X(2).
            03 EPRODA                         PIC X.
            03 EPRODH                         PIC X.
            03 EPRODO                         PIC X(3).
            03 FILLER                         PIC X(2).
            03 AFCFBA                         PIC X.
            03 AFCFBH                         PIC X.
            03 AFCFBO                         PIC X(2).
            03 FILLER                         PIC X(2).
            03 AFABFVA                        PIC X.
            03 AFABFVH                        PIC X.
            03 AFABFVO                        PIC X(10).
            03 FILLER                         PIC X(2).
            03 AFABFBA                        PIC X.
            03 AFABFBH                        PIC X.
            03 AFABFBO                        PIC X(10).
            03 FILLER                         PIC X(2).
            03 APWAEBKA                       PIC X.
            03 APWAEBKH                       PIC X.
            03 APWAEBKO                       PIC X(4).
            03 DFHG001O OCCURS 11.
               05 FILLER                         PIC X(2).
               05 EFCZA                          PIC X.
               05 EFCZH                          PIC X.
               05 EFCZO                          PIC X(1).
               05 FILLER                         PIC X(2).
               05 APLUSA                         PIC X.
               05 APLUSH                         PIC X.
               05 APLUSO                         PIC X(1).
               05 FILLER                         PIC X(2).
               05 AVNRA                          PIC X.
               05 AVNRH                          PIC X.
               05 AVNRO                          PIC X(15).
               05 FILLER                         PIC X(2).
               05 AVSTATA                        PIC X.
               05 AVSTATH                        PIC X.
               05 AVSTATO                        PIC X(4).
               05 FILLER                         PIC X(2).
               05 APRODS1A                       PIC X.
               05 APRODS1H                       PIC X.
               05 APRODS1O                       PIC X(3).
               05 FILLER                         PIC X(2).
               05 ANETBTRA                       PIC X.
               05 ANETBTRH                       PIC X.
               05 ANETBTRO                       PIC X(13).
               05 FILLER                         PIC X(2).
               05 AUSTBTRA                       PIC X.
               05 AUSTBTRH                       PIC X.
               05 AUSTBTRO                       PIC X(13).
               05 FILLER                         PIC X(2).
               05 AFAEVDAA                       PIC X.
               05 AFAEVDAH                       PIC X.
               05 AFAEVDAO                       PIC X(10).
               05 FILLER                         PIC X(2).
               05 AFAEBDAA                       PIC X.
               05 AFAEBDAH                       PIC X.
               05 AFAEBDAO                       PIC X(10).
            03 FILLER                         PIC X(2).
            03 AFCZANZA                       PIC X.
            03 AFCZANZH                       PIC X.
            03 AFCZANZO                       PIC X(79).


