      *****************************************************************
      *    COPY-MEMBER: DBPA0072                                   
      *                                                              
      *   <Maske zu PA05 (Anfordern von Ausgleichsansprüchen)      
      *                                                                
      *                                                              
      *                                                              
      *****************************************************************
      *****************************************************************
      * LOGBUCH:                                                      *
      * AUFTRAG    ! DATUM  ! AENDERUNG                    ! NAME     *
      * --------------------------------------------------------------*
      * DSY-4043   !20.09.21! NEU                          ! T13EX67  *
      *            !        !                              !          *
      *            !        !                              !          *
      *                                                               *
      * ENDE-LOGBUCH                                                  *
      *   MICRO FOCUS ENTERPRISE DEVELOPER FOR ECLIPSE  5.0.00256
      *   MICRO FOCUS BMS SCREEN PAINTER (VER BMSIDE_5-0-01)
      *   MAPSET NAME   BPA0072
      *   DATE CREATED  11/09/2021
      *   TIME CREATED  14:40:16

      *  INPUT DATA FOR MAP BPA0072
         01 BPA0072I.
            03 FILLER                         PIC X(12).
            03 AVBNRFBL                       PIC S9(4) COMP.
            03 AVBNRFBF                       PIC X.
            03 FILLER                         PIC X(1).
            03 AVBNRFBI                       PIC X(16).
            03 EVBNRL                         PIC S9(4) COMP.
            03 EVBNRF                         PIC X.
            03 FILLER                         PIC X(1).
            03 EVBNRI                         PIC X(7).
            03 AVBNAMEL                       PIC S9(4) COMP.
            03 AVBNAMEF                       PIC X.
            03 FILLER                         PIC X(1).
            03 AVBNAMEI                       PIC X(40).
            03 ADATUML                        PIC S9(4) COMP.
            03 ADATUMF                        PIC X.
            03 FILLER                         PIC X(1).
            03 ADATUMI                        PIC X(10).
            03 ADATVL                         PIC S9(4) COMP.
            03 ADATVF                         PIC X.
            03 FILLER                         PIC X(1).
            03 ADATVI                         PIC X(5).
            03 EDATVTTL                       PIC S9(4) COMP.
            03 EDATVTTF                       PIC X.
            03 FILLER                         PIC X(1).
            03 EDATVTTI                       PIC X(2).
            03 EDATVMML                       PIC S9(4) COMP.
            03 EDATVMMF                       PIC X.
            03 FILLER                         PIC X(1).
            03 EDATVMMI                       PIC X(2).
            03 EDATVJJL                       PIC S9(4) COMP.
            03 EDATVJJF                       PIC X.
            03 FILLER                         PIC X(1).
            03 EDATVJJI                       PIC X(4).
            03 ADATBL                         PIC S9(4) COMP.
            03 ADATBF                         PIC X.
            03 FILLER                         PIC X(1).
            03 ADATBI                         PIC X(4).
            03 EDATBTTL                       PIC S9(4) COMP.
            03 EDATBTTF                       PIC X.
            03 FILLER                         PIC X(1).
            03 EDATBTTI                       PIC X(2).
            03 EDATBMML                       PIC S9(4) COMP.
            03 EDATBMMF                       PIC X.
            03 FILLER                         PIC X(1).
            03 EDATBMMI                       PIC X(2).
            03 EDATBJJL                       PIC S9(4) COMP.
            03 EDATBJJF                       PIC X.
            03 FILLER                         PIC X(1).
            03 EDATBJJI                       PIC X(4).
            03 AEDNAMEL                       PIC S9(4) COMP.
            03 AEDNAMEF                       PIC X.
            03 FILLER                         PIC X(1).
            03 AEDNAMEI                       PIC X(6).      
            03 EEDNRL                         PIC S9(4) COMP.
            03 EEDNRF                         PIC X.
            03 FILLER                         PIC X(1).
            03 EEDNRI                         PIC X(7).            
            03 DFHG001I                       OCCURS 10 TIMES.

               05 EFCZL                          PIC S9(4) COMP.
               05 EFCZF                          PIC X.
               05 FILLER                         PIC X(1).
               05 EFCZI                          PIC X(1).
               05 AAENDKZL                       PIC S9(4) COMP.
               05 AAENDKZF                       PIC X.
               05 FILLER                         PIC X(1).
               05 AAENDKZI                       PIC X(1).
               05 ETVBNRL                        PIC S9(4) COMP.
               05 ETVBNRF                        PIC X.
               05 FILLER                         PIC X(1).
               05 ETVBNRI                        PIC X(7).
               05 ETDVTTL                        PIC S9(4) COMP.
               05 ETDVTTF                        PIC X.
               05 FILLER                         PIC X(1).
               05 ETDVTTI                        PIC X(2).
               05 ETDVMML                        PIC S9(4) COMP.
               05 ETDVMMF                        PIC X.
               05 FILLER                         PIC X(1).
               05 ETDVMMI                        PIC X(2).
               05 ETDVJJL                        PIC S9(4) COMP.
               05 ETDVJJF                        PIC X.
               05 FILLER                         PIC X(1).
               05 ETDVJJI                        PIC X(4).
               05 ETDBTTL                        PIC S9(4) COMP.
               05 ETDBTTF                        PIC X.
               05 FILLER                         PIC X(1).
               05 ETDBTTI                        PIC X(2).
               05 ETDBMML                        PIC S9(4) COMP.
               05 ETDBMMF                        PIC X.
               05 FILLER                         PIC X(1).
               05 ETDBMMI                        PIC X(2).
               05 ETDBJJL                        PIC S9(4) COMP.
               05 ETDBJJF                        PIC X.
               05 FILLER                         PIC X(1).
               05 ETDBJJI                        PIC X(4).
               05 ETKUARTL                       PIC S9(4) COMP.
               05 ETKUARTF                       PIC X.
               05 FILLER                         PIC X(1).
               05 ETKUARTI                       PIC X(1).

            03 AFCFBL                         PIC S9(4) COMP.
            03 AFCFBF                         PIC X.
            03 FILLER                         PIC X(1).
            03 AFCFBI                         PIC X(40).

      *  OUTPUT DATA FOR MAP BPA0072
         01 BPA0072O REDEFINES BPA0072I.
            03 FILLER                         PIC X(12).
            03 FILLER                         PIC X(2).
            03 AVBNRFBA                       PIC X.
            03 AVBNRFBH                       PIC X.
            03 AVBNRFBO                       PIC X(16).
            03 FILLER                         PIC X(2).
            03 EVBNRA                         PIC X.
            03 EVBNRH                         PIC X.
            03 EVBNRO                         PIC X(7).
            03 FILLER                         PIC X(2).
            03 AVBNAMEA                       PIC X.
            03 AVBNAMEH                       PIC X.
            03 AVBNAMEO                       PIC X(40).
            03 FILLER                         PIC X(2).
            03 ADATUMA                        PIC X.
            03 ADATUMH                        PIC X.
            03 ADATUMO                        PIC X(10).
            03 FILLER                         PIC X(2).
            03 ADATVA                         PIC X.
            03 ADATVH                         PIC X.
            03 ADATVO                         PIC X(5).
            03 FILLER                         PIC X(2).
            03 EDATVTTA                       PIC X.
            03 EDATVTTH                       PIC X.
            03 EDATVTTO                       PIC X(2).
            03 FILLER                         PIC X(2).
            03 EDATVMMA                       PIC X.
            03 EDATVMMH                       PIC X.
            03 EDATVMMO                       PIC X(2).
            03 FILLER                         PIC X(2).
            03 EDATVJJA                       PIC X.
            03 EDATVJJH                       PIC X.
            03 EDATVJJO                       PIC X(4).
            03 FILLER                         PIC X(2).
            03 ADATBA                         PIC X.
            03 ADATBH                         PIC X.
            03 ADATBO                         PIC X(4).
            03 FILLER                         PIC X(2).
            03 EDATBTTA                       PIC X.
            03 EDATBTTH                       PIC X.
            03 EDATBTTO                       PIC X(2).
            03 FILLER                         PIC X(2).
            03 EDATBMMA                       PIC X.
            03 EDATBMMH                       PIC X.
            03 EDATBMMO                       PIC X(2).
            03 FILLER                         PIC X(2).
            03 EDATBJJA                       PIC X.
            03 EDATBJJH                       PIC X.
            03 EDATBJJO                       PIC X(4).
            03 FILLER                         PIC X(2).
            03 AEDNAMEA                       PIC X.
            03 AEDNAMEH                       PIC X.
            03 AEDNAMEO                       PIC X(6).
            03 FILLER                         PIC X(2).
            03 EEDNRA                         PIC X.
            03 EEDNRH                         PIC X.
            03 EEDNRO                         PIC X(7).            

            03 DFHG001O OCCURS 10.
               05 FILLER                         PIC X(2).
               05 EFCZA                          PIC X.
               05 EFCZH                          PIC X.
               05 EFCZO                          PIC X(1).
               05 FILLER                         PIC X(2).
               05 AAENDKZA                       PIC X.
               05 AAENDKZH                       PIC X.
               05 AAENDKZO                       PIC X(1).
               05 FILLER                         PIC X(2).
               05 ETVBNRA                        PIC X.
               05 ETVBNRH                        PIC X.
               05 ETVBNRO                        PIC X(7).
               05 FILLER                         PIC X(2).
               05 ETDVTTA                        PIC X.
               05 ETDVTTH                        PIC X.
               05 ETDVTTO                        PIC X(2).
               05 FILLER                         PIC X(2).
               05 ETDVMMA                        PIC X.
               05 ETDVMMH                        PIC X.
               05 ETDVMMO                        PIC X(2).
               05 FILLER                         PIC X(2).
               05 ETDVJJA                        PIC X.
               05 ETDVJJH                        PIC X.
               05 ETDVJJO                        PIC X(4).
               05 FILLER                         PIC X(2).
               05 ETDBTTA                        PIC X.
               05 ETDBTTH                        PIC X.
               05 ETDBTTO                        PIC X(2).
               05 FILLER                         PIC X(2).
               05 ETDBMMA                        PIC X.
               05 ETDBMMH                        PIC X.
               05 ETDBMMO                        PIC X(2).
               05 FILLER                         PIC X(2).
               05 ETDBJJA                        PIC X.
               05 ETDBJJH                        PIC X.
               05 ETDBJJO                        PIC X(4).
               05 FILLER                         PIC X(2).
               05 ETKUARTA                       PIC X.
               05 ETKUARTH                       PIC X.
               05 ETKUARTO                       PIC X(1).

            03 FILLER                         PIC X(2).
            03 AFCFBA                         PIC X.
            03 AFCFBH                         PIC X.
            03 AFCFBO                         PIC X(40).


 
      *****************************************************************
      *---------------------- ENDE DBPA0072 --------------------------* 
      *****************************************************************
